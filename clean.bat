:: Erases all compiled files
@echo off

SET LIBS=(shared worker_server controller apps\nbody apps\word_count apps\grades apps\transaction_track)

FOR %%x IN %LIBS% DO (
  FOR /D %%y IN (*.cmo *.cmi *.cma *.exe) DO (
    DEL /F "%%y"
  )
)
