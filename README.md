This repository contains an experimental, very simple Pascal compiler created in F#, called FunPascal (short name : fnp). 

Here is simple guide how to compile compiler and run tests (current version is available only for Windows):

1. install the latest .NET SDK
2. start terminal
3. `cd` to root folder of this funpascal repository
4. run `dotnet tool restore`
5. run `dotnet paket install`
6. run `dotet build`
7. run `cd fnp\bin\Debug\net7`
8. run `./fnp.exe --testall`
9. for detailed output you can check the directory `test\intrinsics\out` for `*.elg` and `*.log` files

if you want to test specific case then you can run `./fnp.exe --test testfile.pas` or `./fnp.exe --testall <someDirWithSetOfTests>`

How to create tests is explained in [test readme.md](test\readme.md)

FunPascal in its current stage is very limited but it works and it can compile another Pascal compiler called XDPW (and this compiled compiler in FunPascal compiler works well). I was very interested how will looks like simple compiler created by using existing libraries like FParsec and dnlib in F#. The result IMO is not bad. Main purpose of this project was learning F#, which was an amazing experience. When I started, I had an extreme problem compiling even simple functional F# code just to print some values on the screen (sic!), but with a lot of patience and "unlearning" imperative/OO programming skills, the real fun with F# started. I feel much more free. Thanks to F# I am a better C# programmer, much more open minded. 

Some notes:

* At the moment the compiler uses RTL from XD Pascal.
* In the future I had planned to use part (or even large part) of RTL from FreePascal
* The main goal of this project is fun(ctional) programming and border crossing :)!
* One of the goals is to create a compiler more like C++/CLI which should be able to compile most of existing Pascal code (like Lazarus and mORMot code base) and additionally use of .NET. This will not be pure .NET oriented compiler.
* At some point I want to implement mselang dialect, but priority is Delphi mode from FreePascal
* This is a long term never ending project, for now the main problem is time. If you like my idea you can sponsor me (or not).
* License of this project is GPL v3 but probably will be changed to more liberal MIT