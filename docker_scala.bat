@echo off
docker run -it --rm --network=none --volume=F:/VectoRiscV:/data spinalhdl sbt --batch -Dsbt.server.forcestart=true "runMain vexriscv.aradex.VpSocVerilog"
echo FINISHED!
xcopy temp\VpSoc\VpSoc.v F:\20220425_VexRiscV\velogic\cpu\vexriskv_cpu\ /Y
pause