# 4190.310 Programming Languages

Language interpreters and type systems implementation from [4190.310 PL homeworks](http://kwangkeunyi.snu.ac.kr/4190.310/22/) and [lecture notes](http://kwangkeunyi.snu.ac.kr/4190.310/11/pl-book-draft.pdf).



|      |                                      |
|------|--------------------------------------|
|[KMinus](./kminus/)|imperative K- interpreter|
|(WIP) [SM5 + GC](./sm5/)|Language translation, virtual machine, garbage collection| 
|[M](./m/)|M interpreter|
|[Simple type system](./simple/)| M simple type system|
|[Polymorphic type system](./poly/)|M let-polymorphic type system|


## Run test

``` shell
$ gem install curses parallel nokogiri  # prerequisites for testting

$ ./test # run test
```

Test code and test cases refer to [simnalamburt/snucse.pl](https://github.com/simnalamburt/snucse.pl/blob/master/test)

