open System.Text
open Aardvark.Base
open Aardvark.Rendering
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Application
open System.Runtime.InteropServices
open Aardvark.Data.Opc
open MBrace.FsPickler
open Aardvark.Geometry

#nowarn "9"
#nowarn "8989"


module Serialization =

  let registry = new CustomPicklerRegistry()    
  let cache = PicklerCache.FromCustomPicklerRegistry registry    

  let binarySerializer = FsPickler.CreateBinarySerializer(picklerResolver = cache)
  //let jsonSerializer = FsPickler.CreateJsonSerializer(indent=true)

module Discover = 
  open System.IO
  
  /// <summary>
  /// checks if "path" is a valid opc folder containing "images", "patches", and patchhierarchy.xml
  /// </summary>
  let isOpcFolder (path : string) = 
      let imagePath = Path.combine [path; "images"]
      let patchPath = Path.combine [path; "patches"]
      (Directory.Exists imagePath) &&
      (Directory.Exists patchPath) && 
       File.Exists(patchPath + "\\patchhierarchy.xml")
  
  /// <summary>
  /// checks if "path" is a valid surface folder
  /// </summary>        
  let isSurfaceFolder (path : string) =
      Directory.GetDirectories(path) |> Seq.forall isOpcFolder
  
  let discover (p : string -> bool) path : list<string> =
    if Directory.Exists path then
      Directory.EnumerateDirectories(path, "*", SearchOption.AllDirectories)                     
        |> Seq.filter p            
        |> Seq.toList
    else List.empty
  
  /// returns all valid surface folders in "path"   
  let discoverSurfaces path = 
    discover isSurfaceFolder path          
  
  let discoverOpcs path = 
    discover isOpcFolder path


type PatchLodTree(globalCenter : V3d, opc : OpcPaths, root : option<ILodTreeNode>, parent : option<ILodTreeNode>, level : int, tree : QTree<Patch>) as this =
    static let source = Symbol.Create "Disk"
    let patch, isLeaf =
        match tree with
        | QTree.Leaf p -> p, true
        | QTree.Node(p,_) -> p, false

    let cell = Cell patch.info.GlobalBoundingBox


    let localBounds = patch.info.GlobalBoundingBox.Translated(-globalCenter)
          
    let data = lazy (Patch.load opc ViewerModality.XYZ patch.info)

    let getGeometry() = 
        let g, _ = data.Value
        g

    static let isOrtho (proj : Trafo3d) = proj.Forward.R3.ApproximateEquals(V4d.OOOI,1E-8)
  
    static let fov (proj : Trafo3d) =
        2.0 * atan(proj.Backward.M00) * Constant.DegreesPerRadian

    let equivalentAngle60 (view : Trafo3d) (proj : Trafo3d) =
        if isOrtho proj then 
            let width = proj.Backward.M00 * 2.0 
            let avgPointDistance = patch.triangleSize //localBounds.Size.NormMax / 40.0

            60.0 * avgPointDistance / width
        else 
            let cam = view.Backward.C3.XYZ

            let avgPointDistance = patch.triangleSize //localBounds.Size.NormMax / 40.0

            let minDist = localBounds.GetMinimalDistanceTo(cam)
            let minDist = max 0.01 minDist

            let angle = Constant.DegreesPerRadian * atan2 avgPointDistance minDist

            let fov = fov proj

            60.0 * angle / fov
        
    let children() =
        //lazy (
            match tree with
                | QTree.Node(_, cs) ->
                    cs |> Array.map (fun c -> 
                        let root = 
                            match root with
                            | Some r -> r
                            | None -> this :> ILodTreeNode
                        PatchLodTree(globalCenter, opc, Some root, Some (this :> ILodTreeNode), level + 1, c) :> ILodTreeNode
                    )
                | QTree.Leaf _ -> 
                    Array.empty
        //)

    member x.Patch = patch
    member x.GlobalCenter = globalCenter

    override x.GetHashCode() =
        HashCode.Combine(Unchecked.hash globalCenter, Unchecked.hash patch)

    override x.Equals o =
        match o with
        | :? PatchLodTree as o ->
            o.Patch = patch && globalCenter = o.GlobalCenter
        | _ ->
            false


    interface ILodTreeNode with
        member this.Acquire() = ()
        member this.Release() = ()
        member this.Root = 
            match root with
            | Some r -> r
            | None -> this :> ILodTreeNode
        member this.Parent = parent
        member this.Level = level
        member this.Id = patch.info.Name :> obj
        member this.Name = patch.info.Name
        member this.TotalDataSize = getGeometry().IndexArray.Length / 3
        member this.DataSize = getGeometry().IndexArray.Length / 3
        member this.WorldBoundingBox = patch.info.GlobalBoundingBox

        member this.Cell = cell
        member this.WorldCellBoundingBox = patch.info.GlobalBoundingBox
        member this.DataSource = source

        member this.Children = children() :> seq<_>

        member this.DataTrafo = 
            Trafo3d.Translation(globalCenter)

        member this.GetData(ct, inputs) = 
            let g = getGeometry()

            let positions = g.IndexedAttributes.[DefaultSemantic.Positions] |> unbox<V3f[]>
            let tc = g.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates] |> unbox<V2f[]>

            let trafo = 
                (patch.info.Local2Global * Trafo3d.Translation(-globalCenter)).Forward

            for i in 0 .. positions.Length - 1 do
                positions.[i] <- trafo.TransformPos (V3d positions.[i]) |> V3f

            let positions =
                g.IndexArray |> unbox<int[]> |> Array.map (fun i -> positions.[i])
                
            let tc =
                g.IndexArray |> unbox<int[]> |> Array.map (fun i -> tc.[i])

            let g = 
                IndexedGeometry(
                    Mode = IndexedGeometryMode.TriangleList,
                    IndexedAttributes =
                        SymDict.ofList [
                            DefaultSemantic.Positions, positions :> System.Array
                            DefaultSemantic.DiffuseColorCoordinates, tc :> System.Array
                        ]
                )

            let img =
                let path = Patch.extractTexturePath opc patch.info 0
                try
                    PixImage.Load(path).ToPixImage<byte>(Col.Format.RGBA)
                with e ->
                    Log.error "[Opc] %s" e.Message
                    DefaultTextures.checkerboardPix

            let tex = 
                { new INativeTexture with   
                    member x.WantMipMaps = false
                    member x.Format = TextureFormat.Rgba8
                    member x.MipMapLevels = 1
                    member x.Count = 1
                    member x.Dimension = TextureDimension.Texture2D
                    member x.Item
                        with get(slice : int, level : int) = 
                            { new INativeTextureData with
                                member x.Size = V3i(img.Size, 1)
                                member x.SizeInBytes = uint64 img.Volume.Data.LongLength
                                member x.Use (action : nativeint -> 'a) =
                                    let gc = GCHandle.Alloc(img.Volume.Data, GCHandleType.Pinned)
                                    try action (gc.AddrOfPinnedObject())
                                    finally gc.Free()
                            }
                }
                
                //PixTexture2d(PixImageMipMap [| img :> PixImage |], false)

            let uniforms =
                MapExt.ofList [
                    "DiffuseColorTexture", [| tex |] :> System.Array
                ]

            g, uniforms

       
        member x.ShouldSplit (splitfactor : float, quality : float, view : Trafo3d, proj : Trafo3d) =
            
            not isLeaf && equivalentAngle60 view proj > splitfactor / quality

        member x.ShouldCollapse (splitfactor : float, quality : float, view : Trafo3d, proj : Trafo3d) =
            equivalentAngle60 view proj < (splitfactor * 0.75) / quality
            
        member x.SplitQuality (splitfactor : float, view : Trafo3d, proj : Trafo3d) =
            splitfactor / equivalentAngle60 view proj

        member x.CollapseQuality (splitfactor : float, view : Trafo3d, proj : Trafo3d) =
            (splitfactor * 0.75) / equivalentAngle60 view proj

    new(globalCenter : V3d, paths : OpcPaths, p : QTree<Patch>) = PatchLodTree(globalCenter, paths, None, None, 0, p)

let exportToObj (opcPaths : list<string>) (outFolder : string) =
    let total = opcPaths.Length
    Log.line "exporting %d OPC path(s) to %s" total outFolder

    for opcIdx, opcPath in opcPaths |> List.indexed do
        let dirName = System.IO.Path.GetFileName(opcPath)
        let subFolder = Path.combine [outFolder; dirName]
        System.IO.Directory.CreateDirectory(subFolder) |> ignore

        Log.startTimed "[%d/%d] %s" (opcIdx + 1) total dirName

        Log.startTimed "discovering OPCs in %s" opcPath
        let patchHierarchies =
            Discover.discoverOpcs opcPath |> List.map OpcPaths
        Log.stop()

        Log.startTimed "loading %d patch hierarchies" patchHierarchies.Length
        let hs =
            patchHierarchies
            |> List.map (fun p -> p, PatchHierarchy.load Serialization.binarySerializer.Pickle Serialization.binarySerializer.UnPickle p)
        Log.stop()

        let ids = ResizeArray<int>()
        let pos = ResizeArray<V3d>()
        let tc = ResizeArray<V2f>()
        let mutable packing = TexturePacking.empty (V2i(16384, 16384))
        let mutable atlas = PixImage<byte>(Col.Format.RGBA, V2i(16384, 16384))
        let mutable outIndex = 0

        let flush() =
            let fn = Path.combine [subFolder; $"atlas_{outIndex}.png"]

            Log.startTimed "saving atlas %s" fn
            atlas.Save(fn)
            Log.stop()

            let centroid = pos |> Seq.average

            Log.startTimed "saving obj, %d positions" pos.Count
            let centroidFn = Path.combine [subFolder; $"centroid_%d{outIndex}.txt"]
            System.IO.File.WriteAllText(centroidFn, $"%.8f{centroid.X} %.8f{centroid.Y} %.8f{centroid.Z}")
            let objFn = Path.combine [subFolder; $"model_%d{outIndex}.obj"]
            use f = System.IO.File.OpenWrite objFn
            use w = new System.IO.StreamWriter(f, Encoding.UTF8)
            w.WriteLine ($"mtllib model_%d{outIndex}.mtl")
            w.WriteLine ($"usemtl mtl{outIndex}")
            for p in pos do
                let p = p - centroid
                w.WriteLine ($"v %.5f{p.X} %.5f{p.Y} %.5f{p.Z}")
            for t in tc do
                w.WriteLine ($"vt %.5f{t.X} %.5f{t.Y}")
            let mutable i = 0
            while i < ids.Count do
                let i1 = 1 + ids.[i]
                let i2 = 1 + ids.[i + 1]
                let i3 = 1 + ids.[i + 2]
                w.WriteLine ($"f {i1}/{i1} {i2}/{i2} {i3}/{i3}")
                i <- i + 3

            let mtlFn = Path.combine [subFolder; $"model_%d{outIndex}.mtl"]
            let mtlString = $"""
newmtl mtl{outIndex}
Ka 1.0 1.0 1.0
Kd 1.0 1.0 1.0
Ks 0.0 0.0 0.0
d 1.0
Ns 0.0
illum 0
map_Kd atlas_{outIndex}.png
"""
            System.IO.File.WriteAllText(mtlFn, mtlString)

            Log.stop()

            ids.Clear()
            pos.Clear()
            tc.Clear()
            atlas <- PixImage<byte>(Col.Format.RGBA, V2i(16384, 16384))
            packing <- TexturePacking.empty (V2i(16384, 16384))
            outIndex <- outIndex + 1

        let add (path : string) (img : PixImage<byte>) =
            match TexturePacking.tryAdd path img.Size packing with
            | Some p ->
                packing <- p
                p.Used.[path]
            | None ->
                flush()
                match TexturePacking.tryAdd path img.Size packing with
                | Some p ->
                    packing <- p
                    p.Used.[path]
                | None ->
                    failwithf "geht ned texture packing %s" path

        let totalLeaves = hs |> List.sumBy (fun (_,h) -> QTree.getLeaves h.tree |> Seq.length)
        let mutable leafIdx = 0

        for (p,h) in hs do
            let ls = QTree.getLeaves h.tree
            Log.startTimed "processing OPC (%d leaves)" (ls |> Seq.length)
            for l in ls do
                leafIdx <- leafIdx + 1
                Log.startTimed "patch %d/%d  %s" leafIdx totalLeaves l.info.Name

                Log.startTimed "loading geometry and texture"
                let (ig,_) = Patch.load p ViewerModality.XYZ l.info
                let texPath = Patch.extractTexturePath p l.info 0
                let tex = PixImage.Load(texPath).ToPixImage<byte>(Col.Format.RGBA)
                Log.stop()

                let region = add texPath tex
                let uvScale = V2f(tex.Size) / V2f(atlas.Size)
                let uvOffset = V2f(region.Min) / V2f(atlas.Size)
                let remap (tc : V2f) =
                    V2f(tc.X, 1.0f - tc.Y) * uvScale + uvOffset

                let index = ig.IndexArray |> unbox<int[]>

                let inputPos =
                    ig.IndexedAttributes.[DefaultSemantic.Positions]
                    |> unbox<V3f[]>

                let uv =
                    ig.IndexedAttributes.[DefaultSemantic.DiffuseColorCoordinates]
                    |> unbox<V2f[]>

                Log.startTimed "processing %d triangles" (index.Length / 3)
                let cache = Dict<int, int>()
                let posBuffer = ResizeArray()
                let uvBuffer = ResizeArray()
                let idxBuffer = ResizeArray()

                let trafo = l.info.Local2Global

                let getOrAdd (i : int) =
                    let si = cache.GetOrCreate(i, fun _ ->
                        let idx = posBuffer.Count
                        posBuffer.Add(trafo.TransformPos (V3d inputPos.[i]))
                        let uv = remap uv.[i]
                        let uv = V2f(uv.X, 1.0f - uv.Y)
                        uvBuffer.Add(uv)
                        idx)
                    si + pos.Count

                let mutable ai = 0
                let mutable skipped = 0
                while ai < index.Length do
                    let i0 = index.[ai]
                    let i1 = index.[ai + 1]
                    let i2 = index.[ai + 2]

                    if not (Vec.AnyNaN inputPos.[i0] || Vec.AnyNaN inputPos.[i1] || Vec.AnyNaN inputPos.[i2]) then
                        let si0 = getOrAdd i0
                        let si1 = getOrAdd i1
                        let si2 = getOrAdd i2
                        idxBuffer.Add si0
                        idxBuffer.Add si1
                        idxBuffer.Add si2
                    else
                        skipped <- skipped + 1

                    ai <- ai + 3

                if skipped > 0 then Log.warn "skipped %d NaN triangles" skipped
                Log.stop()

                pos.AddRange(posBuffer)
                tc.AddRange(uvBuffer)
                ids.AddRange(idxBuffer)

                let dst = atlas.Volume.SubVolume(V3l(region.Min.X,region.Min.Y,0),V3l(tex.Size.X,tex.Size.Y,4))
                dst.Set(tex.Volume) |> ignore

                Log.stop() // patch
            Log.stop() // OPC

        flush()
        Log.stop() // overall path

    Log.line "done"
            
let mergeObjFiles (inputPaths : list<string>) (outputFile : string) =
    let isLabeledShadow (path : string) =
        let name = System.IO.Path.GetFileNameWithoutExtension path
        if name.EndsWith "_labeled" then
            let basePath = System.IO.Path.Combine(System.IO.Path.GetDirectoryName path, name.[..name.Length - 9] + ".obj")
            System.IO.File.Exists basePath
        else false

    let objFiles =
        inputPaths
        |> List.collect (fun p ->
            if System.IO.Directory.Exists p then
                System.IO.Directory.EnumerateFiles(p, "*.obj", System.IO.SearchOption.AllDirectories)
                |> Seq.filter (fun f -> not (isLabeledShadow f))
                |> Seq.toList
            elif System.IO.File.Exists p && System.IO.Path.GetExtension(p).ToLowerInvariant() = ".obj" then
                if isLabeledShadow p then
                    //Log.warn "skipping labeled shadow: %s" p
                    []
                else [p]
            else
                Log.warn "skipping non-existent path: %s" p
                []
        )
        |> List.distinct

    Log.line "merging %d OBJ file(s) -> %s" objFiles.Length outputFile

    let outDir =
        let d = System.IO.Path.GetDirectoryName outputFile
        if System.String.IsNullOrEmpty d then "." else d
    System.IO.Directory.CreateDirectory outDir |> ignore
    let outBaseName = System.IO.Path.GetFileNameWithoutExtension outputFile

    // Parse MTL file referenced by the OBJ; returns texture paths indexed by material order,
    // matching the order mesh.Materials is populated — avoids any reliance on name uniqueness.
    let loadMaterialTexturePaths (objFile : string) =
        let dir = System.IO.Path.GetDirectoryName objFile
        let paths = ResizeArray<string>()
        let mtllib =
            System.IO.File.ReadLines objFile
            |> Seq.tryPick (fun line ->
                if line.StartsWith "mtllib " then Some (line.Substring(7).Trim()) else None)
        match mtllib with
        | None -> ()
        | Some mtlFile ->
            let mtlPath = System.IO.Path.Combine(dir, mtlFile)
            if System.IO.File.Exists mtlPath then
                let mutable inMaterial = false
                let mutable currentPath = ""
                for line in System.IO.File.ReadLines mtlPath do
                    let line = line.Trim()
                    if line.StartsWith "newmtl " then
                        if inMaterial then paths.Add currentPath
                        inMaterial <- true
                        currentPath <- ""
                    elif line.StartsWith "map_Kd " then
                        currentPath <- System.IO.Path.Combine(dir, line.Substring(7).Trim())
                if inMaterial then paths.Add currentPath
        paths.ToArray()

    let atlasSize = V2i(16384, 16384)
    let mutable packing  = TexturePacking.empty atlasSize
    let mutable atlas    = PixImage<byte>(Col.Format.RGBA, atlasSize)
    let mutable outIndex = 0

    let allPos  = ResizeArray<V3d>()
    let allUv   = ResizeArray<V2f>()
    let facePos = ResizeArray<int>()
    let faceUv  = ResizeArray<int>()
    let mutable centroid = V3d.Zero
    let mutable centroidSet = false

    let flush () =
        if facePos.Count = 0 then () else

        let name    = sprintf "%s_%d" outBaseName outIndex
        let objPath = System.IO.Path.Combine(outDir, name + ".obj")

        let atlasFile = System.IO.Path.Combine(outDir, name + "_atlas.jpg")
        Log.startTimed "saving atlas %s" atlasFile
        atlas.Save atlasFile
        Log.stop()

        Log.startTimed "writing %s (%d verts, %d uvs, %d tris)" objPath allPos.Count allUv.Count (facePos.Count / 3)
        use f = System.IO.File.OpenWrite objPath
        use w = new System.IO.StreamWriter(f, Encoding.UTF8)
        w.WriteLine(sprintf "mtllib %s.mtl" name)
        w.WriteLine(sprintf "usemtl %s" name)
        for p in allPos do
            w.WriteLine(sprintf "v %.8f %.8f %.8f" p.X p.Y p.Z)
        for uv in allUv do
            w.WriteLine(sprintf "vt %.6f %.6f" uv.X uv.Y)
        let mutable fi = 0
        while fi < facePos.Count do
            w.WriteLine(sprintf "f %d/%d %d/%d %d/%d"
                (1 + facePos.[fi])   (1 + faceUv.[fi])
                (1 + facePos.[fi+1]) (1 + faceUv.[fi+1])
                (1 + facePos.[fi+2]) (1 + faceUv.[fi+2]))
            fi <- fi + 3
        Log.stop()

        let mtlFile = System.IO.Path.Combine(outDir, name + ".mtl")
        System.IO.File.WriteAllText(mtlFile, sprintf """newmtl %s
Ka 1.0 1.0 1.0
Kd 1.0 1.0 1.0
Ks 0.0 0.0 0.0
d 1.0
Ns 0.0
illum 0
map_Kd %s_atlas.jpg
""" name name)

        allPos.Clear(); allUv.Clear(); facePos.Clear(); faceUv.Clear()
        atlas   <- PixImage<byte>(Col.Format.RGBA, atlasSize)
        packing <- TexturePacking.empty atlasSize
        outIndex <- outIndex + 1

    let tryPackTexture (path : string) (tex : PixImage<byte>) =
        Log.line "PACKING %A" path
        match TexturePacking.tryAdd path tex.Size packing with
        | Some p -> packing <- p; p.Used.[path]
        | None ->
            flush ()
            match TexturePacking.tryAdd path tex.Size packing with
            | Some p -> packing <- p; p.Used.[path]
            | None   -> failwithf "texture too large for empty atlas: %s" path

    for objFile in objFiles do
        Log.line "loading %s" (System.IO.Path.GetFileName objFile)
        let mesh = Aardvark.Data.Wavefront.ObjParser.Load objFile

        let positions =
            match mesh.Vertices with
            | :? System.Collections.Generic.IList<V3f> as v -> v |> Seq.map V3d |> Seq.toArray
            | :? System.Collections.Generic.IList<V3d> as v -> v |> Seq.toArray
            | :? System.Collections.Generic.IList<V4f> as v -> v |> Seq.map (fun p -> V3d(float p.X, float p.Y, float p.Z)) |> Seq.toArray
            | :? System.Collections.Generic.IList<V4d> as v -> v |> Seq.map (fun p -> V3d(p.X, p.Y, p.Z)) |> Seq.toArray
            | _ -> [||]

        let texCoords =
            if isNull mesh.TextureCoordinates then [||]
            else mesh.TextureCoordinates |> Seq.map Vec.xy |> Seq.toArray

        let matTexPaths = loadMaterialTexturePaths objFile

        // Pass 1: pack each material's texture into the atlas BEFORE adding positions,
        // so any atlas-full flush happens before posBase is set.
        let matRemap = Dict<int, V2f -> V2f>()
        let buildRemap (mi : int) =
            let path = if mi >= 0 && mi < matTexPaths.Length then matTexPaths.[mi] else ""
            if path <> "" && System.IO.File.Exists path then
                let tex    = PixImage.Load(path).ToPixImage<byte>(Col.Format.RGBA)
                let region = tryPackTexture path tex
                let dst    = atlas.Volume.SubVolume(V3l(region.Min.X, region.Min.Y, 0), V3l(tex.Size.X, tex.Size.Y, 4))
                dst.Set(tex.Volume) |> ignore
                let uvScale  = V2f tex.Size  / V2f atlasSize
                let uvOffset = V2f region.Min / V2f atlasSize
                fun (tc : V2f) ->
                    let uv = V2f(tc.X, tc.Y) * uvScale + uvOffset
                    V2f(uv.X, 1.0f - uv.Y)
            else
                Log.warn "no texture for material index %d in %s" mi (System.IO.Path.GetFileName objFile)
                id
        for set in mesh.FaceSets do
            if not (isNull set.MaterialIndices) then
                for mi in set.MaterialIndices |> Seq.distinct do
                    if not (matRemap.ContainsKey mi) then
                        matRemap.[mi] <- buildRemap mi
        
        // Centroid from first non-NaN position across all files
        if not centroidSet then
            match positions |> Array.tryFind (fun p -> not (Vec.AnyNaN p)) with
            | Some p ->
                centroid    <- p
                centroidSet <- true
                let centroidFile = System.IO.Path.Combine(outDir, outBaseName + "_centroid.txt")
                System.IO.File.WriteAllText(centroidFile, sprintf "%.8f %.8f %.8f" centroid.X centroid.Y centroid.Z)
                Log.line "centroid: %.3f %.3f %.3f" centroid.X centroid.Y centroid.Z
            | None -> ()

        let posBase = allPos.Count
        for p in positions do allPos.Add(p - centroid)

        // Pass 2: emit faces; remap UVs per-face using that face's material index.
        // uvCache is keyed by (matIdx, globalTcIdx) since the same vt may need
        // different atlas remapping depending on which material uses it.
        let uvCache = Dict<struct(int * int), int>()
        for set in mesh.FaceSets do
            let iPos = set.VertexIndices
            let iTc  = if isNull set.TexCoordIndices then iPos else set.TexCoordIndices

            let mutable skipped = 0
            for ti in 0 .. set.ElementCount - 1 do
                let fi  = set.FirstIndices.[ti]
                let cnt = set.FirstIndices.[ti + 1] - fi
                if cnt = 3 then
                    let p0 = positions.[iPos.[fi]]
                    let p1 = positions.[iPos.[fi + 1]]
                    let p2 = positions.[iPos.[fi + 2]]
                    if not (Vec.AnyNaN p0 || Vec.AnyNaN p1 || Vec.AnyNaN p2) then
                        let mi    = if isNull set.MaterialIndices then 0 else set.MaterialIndices.[ti]
                        let remap = match matRemap.TryGetValue mi with | true, r -> r | _ -> id
                        for k in 0 .. 2 do
                            let globalTcIdx = iTc.[fi + k]
                            let newUvIdx =
                                uvCache.GetOrCreate(struct(mi, globalTcIdx), fun _ ->
                                    let idx = allUv.Count
                                    allUv.Add(remap texCoords.[globalTcIdx])
                                    idx)
                            facePos.Add(posBase + iPos.[fi + k])
                            faceUv.Add(newUvIdx)
                    else
                        skipped <- skipped + 1
            if skipped > 0 then Log.warn "skipped %d NaN triangles in %s" skipped (System.IO.Path.GetFileName objFile)

    flush ()
    Log.line "done"

[<EntryPoint>]
let main argv =
    //System.Reflection.Assembly.LoadFile @"C:\repo\aardvark.rendering-master\bin\Debug\netstandard2.0\Aardvark.Rendering.Common.dll" |> ignore

    Aardvark.Init()

    mergeObjFiles [
        @"D:\bla\mesh\Hessigheim\Epoch_March2018\Mesh\per-tile\test\51387_542653_20"
        @"D:\bla\mesh\Hessigheim\Epoch_March2018\Mesh\per-tile\test\51387_542658_20"
    ] @"D:\bla\mergeObj\Hess-201803\mergy"
    mergeObjFiles [
        @"D:\bla\mesh\Hessigheim\Epoch_November2018\Mesh\per_tile\test\51386_542652_17"
        @"D:\bla\mesh\Hessigheim\Epoch_November2018\Mesh\per_tile\test\51386_542658_17"
    ] @"D:\bla\mergeObj\Hess-201903\mergy201903"
    mergeObjFiles [
        @"D:\bla\mesh\Hessigheim\Epoch_March2019\Mesh\per_tile\test\51385_542652_22"
        @"D:\bla\mesh\Hessigheim\Epoch_March2019\Mesh\per_tile\test\51385_542658_22"
    ] @"D:\bla\mergeObj\Hess-201811\mergy201811"
    exit 0
    // exportToObj [
    //     @"D:\bla\opc\1257_Perjen_Demo\Perjen_Ost\10_AU"
    //     @"D:\bla\opc\1257_Perjen_Demo\Perjen_Ost\10_RA"
    //     @"D:\bla\opc\1257_Perjen_Demo\Perjen_Ost\10_SpB1"
    // ] @"D:\bla\OpcObj"
    // exit 0 
    

    0
