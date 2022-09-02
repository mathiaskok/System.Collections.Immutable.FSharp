Get-ChildItem -Path .\ -Filter *.nupkg -Recurse -File -Name| ForEach-Object {
  dotnet nuget push $_ --source "github_mathiaskok"
}