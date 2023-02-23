#!/bin/bash
dotnet new console -lang F# -n $1
dotnet sln fsharp2023.sln add $1/$1.fsproj
