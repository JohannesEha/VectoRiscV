@echo off
docker run -it --rm --network=none --volume=F:/VectoRiscV:/data spinalhdl sbt --batch -Dsbt.server.forcestart=true "runMain vexriscv.aradex.VpSocVhdl_vp test"
echo FINISHED!
xcopy temp F:\20220425_VexRiscV\velogic\cpu\vexriskv_cpu\ /E/H/Y
pause