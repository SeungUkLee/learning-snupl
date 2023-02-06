# Simple type system

* [M spec](http://ropas.snu.ac.kr/~kwang/4190.310/22/M.pdf)
* [Simple type system skel code](http://ropas.snu.ac.kr/~ta/4190.310/22/document/Lowfat_skel.zip)
* [Reference Unification algorithm](http://ropas.snu.ac.kr/~dreameye/PL/slide/PL12.pdf)
* [M algorithm docs](./docs/m_algorithm_in_simple_type_system.pdf)
  * M algorithm $\mathcal{M}$ 은 타입 연립 방정식을 만드는 $\mathcal{V}(\Gamma, e, \tau)$ 와 Unification algorithm $\mathcal{U}(u)$ 를 동시에 하는 알고리즘이다. [Reference 5-1simple-type.pdf](http://ropas.snu.ac.kr/~kwang/4190.310/08/5-1simple-type.pdf)
  * > Hindley/Milner let-polymorphic type system 을 처음으로 sound and complete 하게 구현한 type inference 알고리즘으로 표준으로 사용된 Alogrithm $\mathcal{W}$ 가 있다.<br> $\mathcal{M}$ 은 $\mathcal{W}$ 의 오류 보고를 개선한 알고리즘이다. type error 가 있는 경우 $\mathcal{W}$ 보다 빨리 실패하며 type error message 가 더 informative 하다.<br><br>
  [Oukseh Lee and Kwangkeun Yi. Proofs about a Folklore Let-Polymorphic Type Inference Algorithm](https://dl.acm.org/doi/10.1145/291891.291892)   

#### 컴파일 및 실행

```
$ make
$ ./run examples/test1.m
```

#### 파스 트리 출력

```
$ ./run -pp examples/test1.m
```
