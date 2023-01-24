# SM5

> **Note**
> 
> WIP...

* [K-- spec](http://ropas.snu.ac.kr/~ta/4190.310/22/document/K_doc/k--.pdf)
* [SM5 spec](http://kwangkeunyi.snu.ac.kr/4190.310/22/hw6.pdf)
* [SM5 skel code](http://ropas.snu.ac.kr/~ta/4190.310/22/document/SM5_skel.zip)

#### 컴파일 및 SM5 실행

```shell
$ make
$ ./run examples/test1.k--
```

#### K-- 파스 트리 출력하기

```shell
$ ./run -pk examples/test1.k--
```

#### K-- 실행기로 실행하기

```shell
$ ./run -k examples/test1.k--
```

#### 번역된 SM5 프로그램 출력하기

```shell
$ ./run -psm5 examples/test1.k--
```

#### SM5 기계 위에서 디버그 모드로 실행하기

```shell
$ ./run -debug examples/test1.k--
```

#### GC가 달린 SM5로 실행하기.

```shell
$ ./run -gc examples/test1.k--
```
