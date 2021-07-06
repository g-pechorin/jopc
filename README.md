

parser combinatorics, but, for JSON instead of text

right now ... this is [just a single unit test showing the concept](src/test/scala/peterlavalle/jopc/SimpleTest.scala)

```sbt
resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.g-pechorin" % "jopc" % "master"
```

... now includes "YAON" "Coder" and "JSONFile"

## JSONFile

synchronized "query based" interface around JSONObject and File

please don't pass the JSONObject out of the `def apply[O](act: JSONObject => O): O` method

do all changes in the/a `def apply[O](act: JSONObject => O): O` method and they're written to disk!

## Coder

it's an ad-hoc encoder/decoder setup

can/should work fine with JOPC constructs

... i should update it to use JOPC's error system

## YAON (Yet Another Object Notation)

Something of a "work in progress" in that ... it's not "total" and I lack the time to make it such :'(

... but, eventually, I expect it to be "total" with more use.

I pronounce it "yawn" and it's a JSON subset that;

- looks a bit like YAML
- (probably) isn't actually YAML (sorry)
- smaller than JSON
    - easier for humans/me to mentally parse
- more-stable text
    - more-strict indentation policy
    - stores {k:v} in alphabetical order
    - less version-control "thrashing"
- has a "prefix" at the file head
    - ... to mark ... version?
    - it itself *could be* JSON so ... go nuts