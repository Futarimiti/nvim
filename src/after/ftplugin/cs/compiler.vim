if glob('*.csproj', v:true) isnot ''
  let b:dispatch = 'dotnet build'
  let b:start = 'dotnet run -wait=always'
  compiler dotnet
endif
