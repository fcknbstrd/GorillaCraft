unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls3D,
  Gorilla.Control, Gorilla.Mesh, Gorilla.Cube, Gorilla.Viewport,
  FMX.MaterialSources, Gorilla.Material.Default, Gorilla.Material.Shared,
  Gorilla.Material.Atlas, FMX.Types3D, System.Math.Vectors, Gorilla.Light,
  FMX.Objects3D, Gorilla.Camera, FMX.Objects, Gorilla.Terrain.Algorithm,
  Gorilla.Controller.Input.Character, Gorilla.Controller.Input.FirstPerson,
  Gorilla.Controller, Gorilla.Controller.Input, Gorilla.Controller.Input.Types,
  Gorilla.Sphere, Gorilla.Material.Lambert, Gorilla.Controller.Input.ThirdPerson,
  Gorilla.Model, Gorilla.AssetsManager, Gorilla.Animation.Controller,
  Gorilla.Plane, Gorilla.Material.Water, Gorilla.Controller.Passes.Refraction,
  Gorilla.Controller.Passes.Reflection, Gorilla.SkyBox, Gorilla.Controller.Passes.SMStoreDepth,
  Gorilla.Utils.Inventory, Gorilla.UI.Inventory, Gorilla.Transform;

const
  /// Height (from low to high) of the terrain
  TERRAIN_HEIGHT  = 20;

  /// Size of the height map, shall be values of 2 ^ x
  TERRAIN_SIZE = 128;

  /// DO NOT CHANGE: fixed byte value for rgb color values in height map
  MAX_COLOR_VALUE = 256;

  /// Defines the max. number of block types (grass, stone, dirt, water, ...)
  /// Change this value, if you have more materials
  NUMBER_OF_BLOCKTYPES = 4;

  /// Defines the max. number of object/plant types
  /// Change this value, if you have more objects
  NUMBER_OF_OBJECTS = 4;

  /// Defines the maximum distance for marking blocks.
  MAX_BLOCK_MARK_DISTANCE = 8;

type
  PBlockType = ^TBlockType;
  TBlockType = record
    Element   : TGorillaMesh;
    Instances : Integer;
  end;

  TSelectedBlock = record
    BlockTypeRef : PBlockType;
    Position : TPoint3D;
    Coords : TPoint;
    Index : Integer;

    procedure Clear();
  end;

  TForm1 = class(TForm)
    GorillaSharedAtlasMaterialSource1: TGorillaSharedAtlasMaterialSource;
    GorillaViewport1: TGorillaViewport;
    GrassBlock: TGorillaCube;
    GorillaAtlasMaterialSource1: TGorillaAtlasMaterialSource;
    GorillaLight1: TGorillaLight;
    GorillaCamera1: TGorillaCamera;
    DirtBlock: TGorillaCube;
    GorillaAtlasMaterialSource2: TGorillaAtlasMaterialSource;
    WaterBlock: TGorillaCube;
    GorillaAtlasMaterialSource3: TGorillaAtlasMaterialSource;
    StoneBlock: TGorillaCube;
    GorillaAtlasMaterialSource4: TGorillaAtlasMaterialSource;
    Timer1: TTimer;
    HeightMapImage: TImage;
    MarkedBlock: TGorillaCube;
    GorillaLambertMaterialSource1: TGorillaLambertMaterialSource;
    GorillaThirdPersonController1: TGorillaThirdPersonController;
    GorillaInputController1: TGorillaInputController;
    GorillaModel1: TGorillaModel;
    GorillaAssetsManager1: TGorillaAssetsManager;
    GorillaAnimationController1: TGorillaAnimationController;
    WaterSurface: TGorillaPlane;
    GorillaWaterMaterialSource1: TGorillaWaterMaterialSource;
    GorillaRenderPassRefraction1: TGorillaRenderPassRefraction;
    GorillaRenderPassReflection1: TGorillaRenderPassReflection;
    CharacterPosMarker: TCircle;
    GorillaSkyBox1: TGorillaSkyBox;
    GorillaLight2: TGorillaLight;
    GrassPlant: TGorillaPlane;
    GorillaAtlasMaterialSource5: TGorillaAtlasMaterialSource;
    FlowerPlant: TGorillaPlane;
    GorillaAtlasMaterialSource6: TGorillaAtlasMaterialSource;
    MushroomPlant: TGorillaPlane;
    GorillaAtlasMaterialSource7: TGorillaAtlasMaterialSource;
    CactusPlant: TGorillaPlane;
    GorillaAtlasMaterialSource8: TGorillaAtlasMaterialSource;
    GorillaInventory1: TGorillaInventory;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GorillaThirdPersonController1Rotate(ASender: TObject;
      ACurrentRotation, ANewRotation: TQuaternion3D; ADelta: TPointF);
    procedure GorillaThirdPersonController1Move(ASender: TObject;
      AKind: TGorillaCharacterControllerHotKey; ADir: TPoint3D;
      ACurrentSpeed: Single);
    procedure GorillaThirdPersonController1Idle(ASender: TObject;
      AState: TGorillaCharacterControllerState; AMode: TGorillaInputMode);
    procedure GorillaThirdPersonController1Jump(ASender: TObject;
      AState: TGorillaCharacterControllerState; AMode: TGorillaInputMode);
    procedure GorillaViewport1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure GorillaViewport1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    /// Saving mouse states for detecting if adding or removing block
    FStoredShiftState: TShiftState;
    /// Number of total blocks
    FBlockCount : Integer;
    /// A list of all available types of blocks.
    FBlockTypes : TArray<TBlockType>;
    /// The currently selected block with some further information.
    FSelectedBlock : TSelectedBlock;
    /// A list of all available types of plants or objects.
    FObjects : TArray<TBlockType>;
    /// Last ripple coordinate to prevent from adding ripples every millisecond
    FLastRipple : TPoint3D;
    /// The global shadow mapping render pass.
    GorillaShadowMappingPass1 : TGorillaRenderPassSMStoreDepth;

    /// <summary>
    /// Map 3D coordinates to height map 2D coordinates and modifies the APos
    /// argument in Y position.
    /// </summary>
    function DoRequest3DPos(var APos : TPoint3D; ADoModifiyY : Boolean = true) : TPoint;
    /// <summary>
    /// Adjusts the character controller in Y position by calling DoRequest3DPos.
    /// </summary>
    procedure DoAdjustCharacterYPosition(AKind: TGorillaCharacterControllerHotKey;
      ADir: TPoint3D);
    /// <summary>
    /// Is getting called when GorillaThirdPersonController1Rotate detects a
    /// cube for marking. The method casts a ray in camera direction onto all
    /// available blocks.
    /// </summary>
    procedure DoMarkBlock(APos : TPoint3D);
    /// <summary>
    /// In case of adding or removing blocks we need to adjust object's position on top,
    /// otherwise they would fly in the air or would be inside of a block.
    /// </summary>
    procedure DoReplaceObjectAtBlockPos(APos : TPoint3D; AMode : Integer);

    /// <summary>
    /// Returns the platform dependent path to the assets directory.
    /// </summary>
    function GetAssetsDirectory() : String;
    /// <summary>
    /// Returns the, at designtime created, in-memory package for putting
    /// assets data into.
    /// </summary>
    function GetInMemoryPackage() : TGorillaAssetsPackage;
  public
    /// <summary>
    /// Experimental: Creates a shadow mapping render pass and attaches GorillaLight2
    /// to it for casting a shadow for the character.
    /// </summary>
    procedure CreateShadowMapping();
    /// <summary>
    /// Loads the BitBot character model with all animations.
    /// </summary>
    procedure LoadCharacter();

    /// <summary>
    /// Generates a random height by diamond square algorithm and applies it to
    /// the HeightMapImage component.
    /// </summary>
    procedure GenerateHeightMap();
    /// <summary>
    /// Builds virtual cube instances from a previously generated height map.
    /// </summary>
    procedure BuildMap();
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Math, System.Rtti,
  Gorilla.FBX.Loader, Gorilla.Utils.Math, Gorilla.DefTypes, Gorilla.Material.Types;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  LBmp : TGorillaBitmapPoolEntry;
begin
  Randomize();

//  GorillaViewport1.GlobalIllumDetail := 5;
//  GorillaViewport1.GlobalIllumSoftness := 3;
//  GorillaViewport1.ShadowStrength := 1;

  /// Modify fog color
  GorillaViewport1.FogColorF := TAlphaColorF.Create(TAlphaColorRec.White);

  /// Our MarkedBlock shall be a little bit transparent
  MarkedBlock.SetOpacityValue(0.75);

  /// Register all cubes as types. If you add more, you have to extend this array
  /// and change the NUMBER_OF_BLOCKTYPES value too.
  System.SetLength(FBlockTypes, NUMBER_OF_BLOCKTYPES);
  FBlockTypes[0].Element := WaterBlock;
  FBlockTypes[1].Element := GrassBlock;
  FBlockTypes[2].Element := DirtBlock;
  FBlockTypes[3].Element := StoneBlock;

  /// Reset position blocks out of range, because we only want to see the blocks at design time
  for I := Low(FBlockTypes) to High(FBlockTypes) do
    FBlockTypes[I].Element.Position.Y := 100;
  MarkedBlock.Position.Y := 100;

  /// Register all types of plants and objects. If you add more, you have to
  /// expand this array and change NUMBER_OF_OBJECTS value too.
  System.SetLength(FObjects, NUMBER_OF_OBJECTS);
  FObjects[0].Element := GrassPlant;
  FObjects[1].Element := FlowerPlant;
  FObjects[2].Element := MushroomPlant;
  FObjects[3].Element := CactusPlant;

  /// Reset position objects out of range, because we only want to see the blocks at design time
  for I := Low(FObjects) to High(FObjects) do
  begin
    FObjects[I].Element.Position.Y := 100;
    FObjects[I].Element.SetHitTestValue(false);
  end;

  /// After we have defined our material / block types, we can start building
  /// virtual instances from a height map.
  BuildMap();

  /// Load the bitbot character with all animations
  LoadCharacter();

  /// Because our water-surface is using reflection and refraction render passes
  /// we need to adjust it to the water-surface position.
  GorillaRenderPassRefraction1.SurfacePosition.Point := WaterSurface.Position.Point;
  GorillaRenderPassReflection1.MirrorPosition.Point := WaterSurface.Position.Point;

  /// Create a shadow mapping render pass for shadow computation
  CreateShadowMapping();

  /// Adjust camera in Position for a closer focus on our model
  /// Position set at the design time will be overwritten on startup
  GorillaCamera1.Position.Point := Point3D(0, -3.5, -5);

(*
  /// And increase the camera x-axis angle limit
  var LCtx : TRttiContext;
  var LType : TRttiInstanceType;
  var LFld  : TRttiField;
  LType := LCtx.GetType(TGorillaThirdPersonController) as TRttiInstanceType;
  LFld := LType.GetField('FCameraXAngleLimit');
  LFld.SetValue(GorillaThirdPersonController1, TValue.From<TPointF>(PointF(-90, 90)));
*)

  /// Modify water shader foam texture
  LBmp := GorillaWaterMaterialSource1.FindBitmapEntryByName('WaterFoam');
  if Assigned(LBmp) then
  begin
    LBmp.SetTextureMinMagFilter(TTextureFilter.Nearest, TTextureFilter.Nearest);
  end;

  /// When we're ready with creating our world, we enable the input controlller,
  /// to allow movement by the GorillaFirstPersonController1.
  GorillaInputController1.Enabled := true;

  /// Initial character position on terrain
  Self.GorillaThirdPersonController1.Position.Y := -100;
  Self.DoAdjustCharacterYPosition(TGorillaCharacterControllerHotKey.UnknownHotKey,
    TPoint3D.Zero);
end;

function TForm1.GetAssetsDirectory() : String;
var LPath : String;
begin
  /// Return our platform dependent assets directory
{$IFDEF MSWINDOWS}
  LPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
{$ELSE}
  LPath := System.IOUtils.TPath.GetHomePath();
{$ENDIF}

  Result := LPath + IncludeTrailingPathDelimiter('assets')
end;

function TForm1.GetInMemoryPackage() : TGorillaAssetsPackage;
begin
  /// Select and return the existing InMemory package for storing assets data.
  Result := GorillaAssetsManager1.GetPackage('InMemory');
end;

procedure TForm1.CreateShadowMapping();

  /// Bugfix: in 0.8.4.2314 TGorillaRenderPassController.AllowControl only
  /// adds the TGorillaModel instance and not all sub meshes /components
  /// This is workaround to also add those
  procedure AllowControls(APass : TGorillaRenderPassController; ACtrl : TControl3D);
  var LCtrl : TGorillaManagedControl;
      I     : Integer;
      LC3D  : TControl3D;
      LCmp  : TComponent;
  begin
    if not Assigned(APass) then
      Exit;

    if not Assigned(ACtrl) then
      Exit;

    LCtrl := APass.AllowedControls.Add() as TGorillaManagedControl;
    LCtrl.Control := ACtrl;

    /// automatically allow sub meshes (treated as separated instances at render time)
    if ACtrl is TGorillaMesh then
    begin
      /// automatically add sub meshes
      if Assigned(TGorillaMesh(ACtrl).Meshes) then
      begin
        for I := 0 to TGorillaMesh(ACtrl).Meshes.Count - 1 do
        begin
          LC3D := TGorillaMesh(ACtrl).Meshes[I];
          if not APass.AllowedControls.Contains(LC3D) then
            AllowControls(APass, LC3D);
        end;
      end;
    end;

    /// automatically allow sub components (treated as separated instances at render time)
    for I := 0 to ACtrl.ComponentCount - 1 do
    begin
      LCmp := ACtrl.Components[I];
      if (LCmp is TControl3D) then
      begin
        LC3D := TControl3D(LCmp);
        if not APass.AllowedControls.Contains(LC3D) then
          AllowControls(APass, LC3D);
      end;
    end;
  end;

begin
  /// For shadow mapping we need to setup a render pass for the sun light source
  GorillaShadowMappingPass1 := TGorillaRenderPassSMStoreDepth.Create(GorillaViewport1);
  GorillaShadowMappingPass1.Viewport := GorillaViewport1;
  GorillaShadowMappingPass1.Light := GorillaLight2;

  /// We only want to cast a shadow for our character
  AllowControls(GorillaShadowMappingPass1, GorillaModel1);
  AllowControls(GorillaShadowMappingPass1, WaterSurface);

  /// Afterwards we apply that shadow mapping pass to our texture atlas materials.
  GorillaSharedAtlasMaterialSource1.Shadows := true;
  GorillaSharedAtlasMaterialSource1.ShadowMethod := TGorillaShadowMethod.ShadowMapping;
  GorillaSharedAtlasMaterialSource1.ShadowMapPass[0] := GorillaShadowMappingPass1;
end;

procedure TForm1.LoadCharacter();
var LPath : String;
    LOpts : TGorillaLoadOptions;
begin
  /// Get our platform dependent assets directory for the character model
  LPath := GetAssetsDirectory() + IncludeTrailingPathDelimiter('models')
    + IncludeTrailingPathDelimiter('BitBot');

  /// Setup the loading structure with all necessary information
  LOpts := TGorillaLoadOptions.Create(LPath + 'BitBot.fbx', GetInMemoryPackage());

  /// We do not need any lights or cameras from the model
  LOpts.ImportCameras := false;
  LOpts.ImportLights := false;

  /// But we want to add animations to our model
  LOpts.ImportAnimations := true;
  LOpts.AdditionalAnimations := [
    LPath + 'BitBot-idle.fbx',
    LPath + 'BitBot-jump.fbx',
    LPath + 'BitBot-walk-forward.fbx',
    LPath + 'BitBot-walk-backward.fbx',
    LPath + 'BitBot-walk-left.fbx',
    LPath + 'BitBot-walk-right.fbx',
    LPath + 'BitBot-run-forward.fbx',
    LPath + 'BitBot-run-backward.fbx',
    LPath + 'BitBot-run-left.fbx',
    LPath + 'BitBot-run-right.fbx'
    ];

  /// Finally we load it by settings from file
  GorillaModel1.LoadFromFile(LOpts);

  /// After the model and animations were loaded we adjust it to our scene
  GorillaModel1.Scale.Point := Point3D(0.00034, 0.00034, 0.00034) * 2;
  GorillaModel1.RotationAngle.Point := Point3D(180, 180, 0);
  GorillaModel1.Position.Point := Point3D(0, 1.35, 0);

  /// The model shall not be interacting with the mouse, therefore deativate hittesting
  GorillaModel1.SetHitTestValue(false);

  /// Start first animation it can find
  GorillaModel1.AnimationManager.PlayAnimation('BitBot-idle.fbx');
end;

procedure TForm1.GenerateHeightMap();
var LAlg : TDiamondSquareTerrain;
    LBmp : TBitmap;
begin
  /// We can use the diamond square terrain height map generation class.
  /// It easily builds a nice and random terrain height map for us.
  LAlg := TDiamondSquareTerrain.Create();
  try
    /// GetHeightMap() will generate the heightmap.
    LBmp := LAlg.GetHeightMap();
    try
      /// Afterwards we apply it to our visualization and to store it.
      /// Because during runtime we still need to access it.
      HeightMapImage.Bitmap.Assign(LBmp);

      /// The terrain algorithm (TDiamondSquareTerrain) produces a map-size of 512 x 512 px.
      /// We shall scale it down, otherwise our BuildMap() will create 262.144 cube instances.
      /// And this is not working properly on all machines.
      HeightMapImage.Bitmap.Resize(TERRAIN_SIZE, TERRAIN_SIZE);
    finally
      /// GetHeightMap() returns a temporary bitmap instance, so we need to free it.
      FreeAndNil(LBmp);
    end;
  finally
    /// Do not forget to free the algorithm instance afterwards.
    FreeAndNil(LAlg);
  end;
end;

procedure TForm1.BuildMap();

  /// Randomly placing some plants and objects onto the blocks
  /// We use a different random seed depending on the block type
  /// So unterwater and on grass more plants are generated than on higher
  /// block types.
  procedure AddingObjectToBlock(AIdx : Integer; ATransf : TMatrix3D; var APixelColor : TAlphaColor);
  var LSeed    : Integer;
      LObjects : TArray<Integer>;
      LObjIdx  : Integer;
      LObjRef  : PBlockType;
  begin
    case AIdx of
      0 : begin
            // Underwater blocks only with grass plants
            LSeed := 10;
            LObjects := [0];
          end;

      1 : begin
            // Grass blocks only with grass plants and flowers
            LSeed := 10;
            LObjects := [0,1];
          end;

      2 : begin
            // Dirt blocks only with grass plants and mushrooms
            LSeed := 20;
            LObjects := [0,2];
          end;

      else
        begin
          // Stone blocks only with cactus plants
          LSeed := 20;
          LObjects := [3];
        end;
    end;

    if (Random(LSeed) < 5) then
    begin
      /// Select a random object from available types
      LObjIdx := LObjects[ Random(System.Length(LObjects)) ];
      LObjRef := @FObjects[LObjIdx];

      /// Store the object type in the Blue channel of the height map
      TAlphaColorRec(APixelColor).B := LObjIdx;

      /// Placing the object and rotate randomly around y-axis.
      ATransf := TMatrix3D.CreateRotationY(DegToRad(Random(180))) *
        ATransf * TMatrix3D.CreateTranslation(Point3D(0, -1, 0));
      LObjRef^.Element.AddInstance(ATransf);
    end;
  end;

var X, Y, Z : Integer;
    LYScale : Single;
    LWidth,
    LHeight : Integer;
    LHalfWidth,
    LHalfHeight : Integer;
    LIdx    : Integer;
    LPxlClr : TAlphaColor;
    LTerrainHeight,
    LLevels : Integer;
    LOfs    : TPoint3D;
    LMat3D  : TMatrix3D;
    LData   : TBitmapData;
begin
  /// Generate a height map to adjust cube instance to
  GenerateHeightMap();

  /// Lock our heightmap to request pixel information
  if HeightMapImage.Bitmap.Map(TMapAccess.ReadWrite, LData) then
  begin
    /// Compute the level value depending on the number of blocks
    /// The level defines the height range for each type, f.e.:
    /// water = 0 - 63, stone = 64 - 127, ...
    LLevels := MAX_COLOR_VALUE div NUMBER_OF_BLOCKTYPES;

    /// Set the low-to-high height range, because without scaling height, it would
    /// be from -255 to +255, which is way to large
    LTerrainHeight := TERRAIN_HEIGHT;
    try
      LWidth  := LData.Width;
      LHeight := LData.Height;

      LHalfWidth := LWidth div 2;
      LHalfHeight := LHeight div 2;

      for X := 0 to LWidth - 1 do
      begin
        for Z := 0 to LHeight - 1 do
        begin
          /// Request the pixel value.
          LPxlClr := LData.GetPixel(X, Z);
          /// Because it is black-and-white texture, we only need the RED channel.
          Y := TAlphaColorRec(LPxlClr).R;

          /// Get a ratio value for this pixel
          LYScale := Y / MAX_COLOR_VALUE;

          /// Get the blocktype depending on the height value of the terrain
          /// LLevels defines the steps in height for each block type, f.e.
          /// water = 0 - 63, stone = 64 - 127, ...
          /// By increasing NUMBER_OF_BLOCKTYPES and adding more, we can get more detail
          LIdx := Floor(Y / LLevels);

          /// Setup the position for the instance of the block type.
          LOfs.X := -LHalfWidth + X;
          LOfs.Z := LHalfHeight - Z;
          LOfs.Y := (LTerrainHeight div 2) - Round(LTerrainHeight * LYScale);

          /// Modify terrain value to level values
          TAlphaColorRec(LPxlClr).R := Ceil( ((LTerrainHeight * LYScale) / LTerrainHeight) * MAX_COLOR_VALUE );

          /// Each instance needs its own absolute matrix information
          /// even if only the translation is interesting here.
          /// That's why we use the FBlockTypes[LIdx].AbsoluteMatrix as basis.
          LMat3D := TMatrix3D.CreateScaling(Point3D(1, 1, 1)) * TMatrix3D.CreateTranslation(LOfs);
          FBlockTypes[LIdx].Element.AddInstance(LMat3D);

          /// Count instances for each blocktype, for raycasting in GorillaFirstPersonController1Rotate
          /// to detect which block is selected.
          Inc(FBlockTypes[LIdx].Instances);

          /// Just for some statistics, we count the blocks we're adding
          Inc(FBlockCount);

          /// Store block type in heightmap (G-Channel)
          TAlphaColorRec(LPxlClr).G := LIdx;

          /// Randomly placing some plants and objects onto the blocks
          /// We use a different random seed depending on the block type
          /// So unterwater and on grass more plants are generated than on higher
          /// block types.
          AddingObjectToBlock(LIdx, LMat3D, LPxlClr);

          /// Finally we store the modified pixel color again
          LData.SetPixel(X, Z, LPxlClr);
        end;
      end;
    finally
      /// Finally unlock our bitmap
      HeightMapImage.Bitmap.Unmap(LData);
    end;
  end;
end;

function TForm1.DoRequest3DPos(var APos : TPoint3D; ADoModifiyY : Boolean = true) : TPoint;
var LData   : TBitmapData;
    LTerrainHeight : Integer;
    LHalfWidth,
    LHalfHeight,
    LWidth,
    LHeight : Integer;
    LPxlClr : TAlphaColor;
    Y       : Single;
    LYScale : Single;
begin
  Result := TPoint.Zero;

  /// Lock our heightmap to request pixel information
  if HeightMapImage.Bitmap.Map(TMapAccess.Read, LData) then
  begin
    /// Set the low-to-high height range, because without scaling height, it would
    /// be from -255 to +255, which is way to large
    LTerrainHeight := TERRAIN_HEIGHT;
    try
      LWidth  := LData.Width;
      LHeight := LData.Height;

      LHalfWidth := LWidth div 2;
      LHalfHeight := LHeight div 2;

      /// While 3D world center is in the middle, the center in height map
      /// is at (HalfWidth, HalfHeight) - so, we need to shift our coordinates
      Result.X := Round(APos.X) + LHalfWidth;
      /// The same for the z coordinate, which is the y axis in height map.
      /// But we have to flip coordinate to get correct value from height map.
      Result.Y := LHeight - (Round(APos.Z) + LHalfHeight);

      /// Limit our computed coordinates, to prevent from invalid pixel coordinates.
      Result.X := Clamped(Result.X, 0, LWidth);
      Result.Y := Clamped(Result.Y, 0, LHeight);

      /// Request the pixel value.
      LPxlClr := LData.GetPixel(Result.X, Result.Y);

      /// Because it is black-and-white texture, we only need the RED channel.
      Y := TAlphaColorRec(LPxlClr).R;
      /// get a ratio value for this pixel
      LYScale := Y / MAX_COLOR_VALUE;
      /// apply the ratio value to our max. terrain height incl. the lower offset.
      if ADoModifiyY then
        Y := (LTerrainHeight div 2) - Round(LTerrainHeight * LYScale) - 1
      else
        Y := (LTerrainHeight div 2) - Round(LTerrainHeight * LYScale);

      /// finally apply y position to our first person controller
      APos.Y := Y;
    finally
      /// Finally unlock our bitmap
      HeightMapImage.Bitmap.Unmap(LData);
    end;
  end;
end;

procedure TForm1.DoAdjustCharacterYPosition(AKind: TGorillaCharacterControllerHotKey;
  ADir: TPoint3D);
var LPrevPos,
    LCamPos   : TPoint3D;
    LCoords   : TPoint;
    LWaterPos : TPoint3D;
begin
  /// When our first person controller move we adjust the Y position of it (GorillaFirstPersonController1)
  /// This is done by transfering our 3D world position to the height map.
  /// After retrieving the correct pixel in height map, we need to transfer
  /// the height value back to a 3D world coordinate.
  LCamPos  := GorillaThirdPersonController1.Position.Point;
  LPrevPos := LCamPos;
  LCoords  := DoRequest3DPos(LCamPos);

  /// Do not adjust, if height difference is too large
  /// We don't want the character to jump directly onto blocks in large height
  if ((LCamPos.Y - LPrevPos.Y) < -1) then
  begin
    /// Handle it as some kind of wall and bounce off
    ADir.Y := 0;
    ADir := ADir * 0.5;
    LCamPos := LPrevPos - ADir;
  end;

  /// Update third person controller position
  GorillaThirdPersonController1.Position.Point := LCamPos;

  /// We have to set the water reflection surface, by our current controller
  /// position and the original water surface y-position
  LWaterPos := LCamPos;
  LWaterPos.Y := WaterSurface.Position.Y;
  GorillaRenderPassReflection1.MirrorPosition.Point := LWaterPos;

  /// Position the map marker
  CharacterPosMarker.Position.Point := PointF(LCoords.X, LCoords.Y);

  /// If water surface was touched, let's create some ripples
  if (LCamPos.Y + 1) > WaterSurface.Position.Y then
  begin
    /// But only add a ripple if distance to last ripple is large enough
    /// otherwise some strange optical effects appear
    if (FLastRipple.Distance(LCamPos) > 0.5) then
    begin
      GorillaWaterMaterialSource1.AddRipple(LCamPos);
      FLastRipple := LCamPos;
    end;
  end;

  /// If character movement switches to MOVE mode, we dispatch this event
  /// to our AnimationController to automatically handle animation switching on
  /// our character model
  /// NOTICE: Check created transitions (designtime) for control.
  GorillaAnimationController1.DispatchInput(AKind, GorillaThirdPersonController1.States);
end;

procedure TForm1.DoMarkBlock(APos : TPoint3D);
begin
  /// Let's place our marking block to see which block is selected.
  MarkedBlock.Position.Point := APos;
end;

procedure TForm1.DoReplaceObjectAtBlockPos(APos : TPoint3D; AMode : Integer);
var I, J : Integer;
    LMat : TMatrix3D;
    LObjPos : TPoint3D;
begin
  for I := Low(FObjects) to High(FObjects) do
  begin
    for J := 0 to FObjects[I].Instances - 1 do
    begin
      LMat := FObjects[I].Element.Instance[J] * TMatrix3D.CreateTranslation(Point3D(0, -AMode * 1, 0));
      LObjPos := TTransformationMatrixUtils.GetTranslationFromTransformationMatrix(LMat);
      if not LObjPos.EqualsTo(APos, 0.0001) then
        Continue;

      FObjects[I].Element.Instance[J] := LMat;
      Exit;
    end;
  end;
end;

procedure TForm1.GorillaThirdPersonController1Idle(ASender: TObject;
  AState: TGorillaCharacterControllerState; AMode: TGorillaInputMode);
begin
  /// If character movement switches to IDLE mode, we dispatch this event
  /// to our AnimationController to automatically handle animation switching on
  /// our character model.
  /// NOTICE: Check created transitions (designtime) for control.
  GorillaAnimationController1.DispatchInput(
    TGorillaCharacterControllerHotKey.UnknownHotKey,
    GorillaThirdPersonController1.States);
end;

procedure TForm1.GorillaThirdPersonController1Jump(ASender: TObject;
  AState: TGorillaCharacterControllerState; AMode: TGorillaInputMode);
begin
  if AMode = TGorillaInputMode.Activated then
    Exit;

  /// After jumping, we shall adjust the y-position
  DoAdjustCharacterYPosition(TGorillaCharacterControllerHotKey.UnknownHotKey,
    TPoint3D.Zero);
end;

procedure TForm1.GorillaThirdPersonController1Move(ASender: TObject;
  AKind: TGorillaCharacterControllerHotKey; ADir: TPoint3D;
  ACurrentSpeed: Single);
begin
  /// Skip y-position adjustment if controller is jumping
  if TGorillaCharacterControllerState.fpJumping in GorillaThirdPersonController1.States then
    Exit;

  /// On moving the character we're adjusting the character y-position
  DoAdjustCharacterYPosition(AKind, ADir);
end;

procedure TForm1.GorillaThirdPersonController1Rotate(ASender: TObject;
  ACurrentRotation, ANewRotation: TQuaternion3D; ADelta: TPointF);
var LCamPos     : TPoint3D;
    LDstPos     : TPoint3D;
    LCamDir     : TPoint3D;
    LOfsDir     : TVector3D;
    I, B        : Integer;
    LBlock      : TGorillaMesh;
    LBlockPos   : TPoint3D;
    LNear, LFar : TPoint3D;
    LIntersect  : Integer;
    LFound      : Boolean;
begin
  /// unselect the previous block
  FSelectedBlock.Clear();

  /// We want to mark the cube we're currently looking at
  /// This is done by projection
  LCamPos := GorillaCamera1.AbsolutePosition.ToPoint3D();
  LCamDir := GetViewDirectionFromViewMatrix(
    GorillaViewport1.MainPassRenderer.SharedContext.CurrentCameraMatrix
    );

  /// To allow some minimal user control of the viewdirection, we move the camera
  /// direction a little bit sideways
  LDstPos := LCamPos + LCamDir;
  LOfsDir := TPoint3D.Zero;
  LDstPos := LDstPos + LOfsDir.ToPoint3D();
  LCamDir := (LCamPos - LDstPos).Normalize();
//  Log.d('dir=(%n, %n, %n), delta=(%n, %n)', [LCamDir.X, LCamDir.Y, LCamDir.Z, ADelta.X, ADelta.Y]);

  /// Because the cubes are not a real 3D instance, we have to check all virtual
  /// cube instances of all block types.
  LFound := false;

  /// Iterate through all block types
  for I := Low(FBlockTypes) to High(FBlockTypes) do
  begin
    LBlock := FBlockTypes[I].Element;

    /// Iterate through all virtual instances of this specific block type.
    for B := 0 to FBlockTypes[I].Instances - 1 do
    begin
      /// For fast computation, we only check the cubes, that are nearby.
      /// We have to extract translation from instance matrix to get virtual
      /// cube position
      LBlockPos := TTransformationMatrixUtils.GetTranslationFromTransformationMatrix(LBlock.Instance[B]);
      /// If cube is more far away, skip it.
      if (Abs(LCamPos.Distance(LBlockPos)) > MAX_BLOCK_MARK_DISTANCE) then
        Continue;

      /// If this cube instance is nearby, we shall perform a raycast onto it,
      /// to check if camera view direction ray intersects with it.
      LIntersect := RayCastCuboidIntersect(LCamPos, LCamDir, LBlockPos, 1, 1, 1, LNear, LFar);
      if LIntersect <> 0 then
      begin
        /// We have found an intersected cube. We can stop searching for more.
        LFound := true;

        /// Let's mark our block for selection.
        FSelectedBlock.BlockTypeRef := @FBlockTypes[I];
        FSelectedBlock.Position := LBlockPos;
        FSelectedBlock.Index := B;
        FSelectedBlock.Coords := Self.DoRequest3DPos(LBlockPos, false);

        DoMarkBlock(LBlockPos);
        Break;
      end;
    end;

    if LFound then
      Break;
  end;
end;

procedure TForm1.GorillaViewport1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FStoredShiftState := Shift;
end;

procedure TForm1.GorillaViewport1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var LTransf : TMatrix3D;
    LData   : TBitmapData;
    LPxClr  : TAlphaColor;
    LMode   : Integer;
    LIgnore : Boolean;
    LLevels : Integer;
begin
  if not Assigned(FSelectedBlock.BlockTypeRef) then
    Exit;

  /// Actions: LeftMouseClick == Add, RightMouseClick == Remove
  if ssLeft in FStoredShiftState then
    LMode := 1
  else if ssRight in FStoredShiftState then
    LMode := -1
  else
    Exit;

  /// Modify height map value
  if HeightMapImage.Bitmap.Map(TMapAccess.ReadWrite, LData) then
  begin
    try
      LPxClr := LData.GetPixel(FSelectedBlock.Coords.X, FSelectedBlock.Coords.Y);

      /// We do not move blocks upwards if we already reached the sky.
      if (LMode >= 1) and (TAlphaColorRec(LPxClr).R >= 255) then
        Exit;

      LLevels := Round(MAX_COLOR_VALUE / TERRAIN_HEIGHT);

      TAlphaColorRec(LPxClr).R := Max(0, Min(TAlphaColorRec(LPxClr).R + LMode * LLevels, 255));
      LData.SetPixel(FSelectedBlock.Coords.X, FSelectedBlock.Coords.Y, LPxClr);
    finally
      HeightMapImage.Bitmap.Unmap(LData);
    end;
  end;

  /// Modify block position
  LTransf := FSelectedBlock.BlockTypeRef^.Element.Instance[FSelectedBlock.Index];
  LTransf := LTransf * TMatrix3D.CreateTranslation(Point3D(0, -LMode, 0));
  FSelectedBlock.BlockTypeRef^.Element.Instance[FSelectedBlock.Index] := LTransf;

  /// Move an object, if it's on top of the block
  DoReplaceObjectAtBlockPos(MarkedBlock.Position.Point, LMode);

  /// Also move the marking block with the repositioned block
  MarkedBlock.Position.Y := MarkedBlock.Position.Y - LMode;

  /// Adjust character if it stood onto of the cube.
  Self.DoAdjustCharacterYPosition(TGorillaCharacterControllerHotKey.UnknownHotKey,
    TPoint3D.Zero);

  FStoredShiftState := [];
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var LAbsPos : TPoint3D;
begin
  /// Showing some statistics for debugging.
  LAbsPos := GorillaThirdPersonController1.Position.Point;
  Self.Caption := Format('Minecraft Demo [FPS: %n, Pos=(%n, %n, %n), Blocks=%d, States=%s]',
    [GorillaViewport1.FPS, LAbsPos.X, LAbsPos.Y, LAbsPos.Z, FBlockCount,
    TGorillaCharacterController.GetStatesString(GorillaThirdPersonController1.States)]);
end;

{ TSelectedBlock }

procedure TSelectedBlock.Clear();
begin
  Self.BlockTypeRef := nil;
  Self.Coords := TPoint.Zero;
  Self.Position := TPoint3D.Zero;
  Self.Index := -1;
end;

end.
