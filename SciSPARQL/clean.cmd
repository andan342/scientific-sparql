del ..\bin\ssdm.dll
del ..\bin\ssdm.lib

devenv MVC\ssdm.sln /clean "Release"

del ..\bin\ssdm.dmp