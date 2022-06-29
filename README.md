# GorillaCraft
GorillaCraft is a tutorial project for demostrating the usage of the Gorilla3D Firemonkey 3D addon for Delphi 10.1.1+

It is based on Minecraft and it's rendering a terrain of blocks by using cube instancing. You are walking through the world by a third person character, controlled by keyboard and mouse. Some additional features are supported, like: shadow, water, particles and more.

The demo is demonstrating various components and how to combine those to build a game like Minecraft.
- TGorillaCube instancing
- Materials like TGorillaLambertMaterialSource, TGorillaSharedAtlasMaterialSource, TGorillaWaterMaterialSource, ...
- TGorillaModel with different animations
- TGorillaThirdPersonController with TGorillaInputController
- TGorillaAnimationController
- TGorillaSkyBox
- TGorillaInventory
- Render passes like: Refraction, Reflection and ShadowMapping
- ...

![Alt text](gorillacraft.jpg?raw=true "GorillaCraft")

We decided to not set up a voxel terrain by mesh data, but instead by rendering virtual GPU instances of different cubes.
Those cubes are stored as meshbuffer inside the GPU (static mesh) and getting rendered only by switching buffers, which is very fast.

Selecting a cube or detecting Y position of a cube works by checking the height map and performing a virtual raytracing.

## Licensing
The demo project is under Creative Commons license. Feel free to expand, fork or whatever.

Gorilla Software provides a free to download developer edition with watermarks and a commercial version without watermarks.

Read more about licensing here: https://docs.gorilla3d.de/0.8.4/pricing

## Requirements
- Delphi IDE 10.1.1+, f.e. 10.4.2 Community Edition: https://www.embarcadero.com/de/products/delphi/starter
- Gorilla3D Firemonkey Addon

## Installation
GorillaCraft __requires__ the Gorilla3D Delphi Firemonkey 3D addon for Delphi 10.1.1+.

__NOTICE:__ For GorillaCraft the latest preview version **0.8.4.2314 is required**.

__NOTICE:__ 0.8.4.2314 preview version is currently only available for 10.4.2, 11.0.0 and 11.1.0. For elder IDE versions, please contact us at: support[at]gorilla3d.de

You can download the package manually or by our installer tool.

### Manual Installation
Download the package suitable to your installed Delphi IDE version: 
https://gorilla3d.de/files/?dir=packages

Please read further information about how to install Gorilla3D manually: 
https://docs.gorilla3d.de/0.8.4/installation

### Installer
You can use the installer tool for automatic installation in Delphi IDE.
https://www.gorilla3d.de/download/Gorilla3DInstaller.zip

## The Project
We would be glad if you participate in this project.

### What has to be done
- Filling up block gaps
- Using TGorillaInventory for choosing block type
- Multiplayer Mode
- Saving / Loading Maps
- ParticleEffects
- Adding a startmenu with some options and settings
