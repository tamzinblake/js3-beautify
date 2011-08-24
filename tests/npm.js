var magicWords = ["abracadabra", "gesundheit", "ventrilo"]
  , spells = { "fireball": function () {
                 setOnFire()
               }
             , "water": function () {
                 putOut()
               }
             }
  , a = 1
  , b = "abc"
  , etc
  , somethingElse


function myThing (args, cb) {
  getData( args
         , function (er, data) {
             if (er) return log.er(cb, "Couldn't get data")(er)
             return doSomethingElse(data, cb)
           }
         )
}

function justHasToWork (cb) {
  doSomething(log.er(cb, "the doSomething failed."))
}
if (foo) bar()
while (foo) bar()
;(x || y).doSomething()
;[a, b, c].forEach(doSomething)
for (var i = 0; i < 10; i++) {
  switch (state) {
    case "begin": start(); continue
    case "end": finish(); break
    default: throw new Error("unknown state")
  }
  end()
}
if ( foo
  && bar
  && baz
  && ( 1
    || 2
    || 3
     )
  && boop
  && verylongthingy
  && somethingelsecrazy
  && thisshouldbelongenough
   ) {
  foo
}

