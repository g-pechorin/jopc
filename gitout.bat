@ECHO OFF

@REM sbt "run -from=../fud14.pure-gen.sbt"
@REM sbt "run -from=../fud14.pure-gen.sbt"

@REM SET run=sbt "run -from=../jopc.sbt"
SET run=sbt "run -live -from=../jopc.sbt"

CMD /C "CD ../gitout.sbt && %run%"
