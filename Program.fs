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


module Shader =
    open FShade
    
    type LodVertex =
        {
            [<Position>] pos : V4f
            [<TexCoord>] tc : V2f
            [<Semantic("ViewPosition")>] vp : V4f
            [<Color>] col : V4f
            [<Normal>] n : V3f
            [<Semantic("TreeId")>] id : int
            [<Semantic("DiffuseColorTextureTrafo")>] textureTrafo : V4f
        }
        
    type UniformScope with
        member x.ModelTrafos : M44f[] = x?StorageBuffer?ModelTrafos
        member x.ModelViewTrafos : M44f[] = x?StorageBuffer?ModelViewTrafos

    let trafo (v : LodVertex) =       
        vertex { 
            let mv = uniform.ModelViewTrafos.[v.id]
            //let f = if magic then 0.07 else 1.0 / 0.3

            let vp = mv * v.pos
            let vn = mv * V4f(v.n, 0.0f) |> Vec.xyz |> Vec.normalize
            let pp = uniform.ProjTrafo * vp

            
            let mutable tc = v.tc
            if tc.X < 0.0f then tc.X <- 0.0f
            elif tc.X > 1.0f then tc.X <- 1.0f
            if tc.Y < 0.0f then tc.Y <- 0.0f
            elif tc.Y > 1.0f then tc.Y <- 1.0f

            tc.Y <- 1.0f - tc.Y

            let tc =
                let trafo = v.textureTrafo
                if trafo.Z < 0.0f then
                    V2f(-trafo.Z, trafo.W) * V2f(1.0f - tc.Y, tc.X) + trafo.XY
                else
                    trafo.ZW * tc + trafo.XY


            return { v with pos = pp; vp = vp; n = vn; tc = tc }

        }
        
    let normals (v : Triangle<LodVertex>) =       
        triangle { 
            let p0 = v.P0.vp.XYZ
            let p1 = v.P1.vp.XYZ
            let p2 = v.P2.vp.XYZ

            let vn = Vec.cross (p1 - p0) (p2 - p0) |> Vec.normalize

            yield { v.P0 with n = vn }
            yield { v.P1 with n = vn }
            yield { v.P2 with n = vn }

        }
        
    let sam =
        sampler2d {
            texture uniform?DiffuseColorTexture
            filter Filter.MinLinearMagPointMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            maxAnisotropy 16
        }



    let light (v : LodVertex) =    
        fragment {
            
            let tc = 
                let mutable tc = V2f(v.tc.X % 1.0f, v.tc.Y % 1.0f)
                if tc.X < 0.0f then tc.X <- 1.0f + tc.X
                if tc.Y < 0.0f then tc.Y <- 1.0f + tc.Y
                tc

            if uniform?MipMaps then
                if uniform?Anisotropic then
                    return sam.Sample(tc)
                else
                    let s = V2f sam.Size
                    let maxLevel = sam.MipMapLevels - 1 |> float32

                    let dx = s * ddx v.tc
                    let dy = s * ddy v.tc

                    let level = log2 (max (Vec.length dx) (Vec.length dy)) |> clamp 0.0f maxLevel

                    return sam.SampleLevel(v.tc, level)
            else
                return sam.SampleLevel(v.tc, 0.0f)
        }

let exportToObj (opcPath : string) (outFolder : string)=
    Log.startTimed "discovering OPCs in %s" opcPath
    let patchHierarchies =
        List.concat [
            Discover.discoverOpcs opcPath
        ] |> List.map OpcPaths
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
        let fn = Path.combine [outFolder; $"atlas_{outIndex}.png"]
        
        Log.startTimed "saving atlas %s" fn
        atlas.Save(fn)
        Log.stop()
        
        let centroid = pos |> Seq.average
        
        Log.startTimed "saving obj %s positions %d" fn pos.Count
        let centroidFn = Path.combine [outFolder; $"centroid_%d{outIndex}.txt"]
        System.IO.File.WriteAllText(centroidFn, $"%.8f{centroid.X} %.8f{centroid.Y} %.8f{centroid.Z}")
        let objFn = Path.combine [outFolder; $"model_%d{outIndex}.obj"]
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
            
            
        let mtlFn = Path.combine [outFolder; $"model_%d{outIndex}.mtl"]
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
    
        
    
    for (p,h) in hs do
        let ls = QTree.getLeaves h.tree
        Log.startTimed "processing OPC (%d leaves)" (ls |> Seq.length)
        for l in ls do
            Log.startTimed "patch %s" l.info.Name

            Log.startTimed "loading geometry and texture"
            let (ig,_) = Patch.load p ViewerModality.XYZ l.info
            let texPath = Patch.extractTexturePath p l.info 0
            let tex = PixImage.Load(texPath).ToPixImage<byte>(Col.Format.RGBA)
            Log.stop()

            let region = add texPath tex
            let uvScale = V2f(tex.Size) / V2f(atlas.Size)
            let uvOffset = V2f(region.Min) / V2f(atlas.Size)
            let remap (tc : V2f) =
                V2f(tc.X, tc.Y) * uvScale + uvOffset


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
                    let uv = V2f.IO - uv
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
            
[<EntryPoint>]
let main argv = 
    //System.Reflection.Assembly.LoadFile @"C:\repo\aardvark.rendering-master\bin\Debug\netstandard2.0\Aardvark.Rendering.Common.dll" |> ignore

    Aardvark.Init()
    
    exportToObj @"D:\bla\opc\VictoriaCrater\HiRISE_VictoriaCrater" @"D:\bla\OpcObj"
    exit 0 
    

    0
