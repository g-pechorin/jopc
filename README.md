just a pile of junk to edit JSON

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
