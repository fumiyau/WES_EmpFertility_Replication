clear all
set more off

global ROOT "`c(pwd)'"
global SSM_DIR "${ROOT}/Data/SSM"
global JGSS_DIR "${ROOT}/Data/JGSS"
global Data_DIR "${ROOT}/Data"
global Tables_DIR "${ROOT}/Tables"

do "${ROOT}/Replication/1.SSM.do"
do "${ROOT}/Replication/2.JGSS.do"
do "${ROOT}/Replication/3.Reshape_SSM.do"
do "${ROOT}/Replication/4.Desc.do"
