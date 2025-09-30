(lang dune 3.5)

(name dep2pict)

(version 2.37.0)

(generate_opam_files true)

(source
 (github grew-nlp/dep2pict))

(authors "Bruno Guillaume")

(maintainers "Bruno.Guillaume@inria.fr")

(license CECILL-2.1)

(package
 (name dep2pict)
 (synopsis "dep2pict")
 (description "drawing dependency trees")
 (depends 
   str
   ANSITerminal 
   containers 
   yojson 
   amr 
   conll
   grewlib
   dep2pictlib
 )
 (tags
  (nlp)
 )
)
