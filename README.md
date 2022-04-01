# How to design program
这是 [How to Design Programs](https://book.douban.com/subject/1787103/)（中文[《程序设计方法 中文版》](https://book.douban.com/subject/1140942/)）的课后习题集。

在这个 `repo` 中，我使用 [Racket](https://racket-lang.org/)（`Scheme` 语言的超集）来实现这些习题。

**这是一个兴趣使然的项目，（起码现在）请不要对这个项目抱有太大期待😓。**

## 1. 说明

install `Racket` on `Ubuntu 20.04 LTS`:

```shell
sudo add-apt-repository ppa:plt/racket
sudo apt-get update
sudo apt-get install racket
```

download `SICP` collection(if you need it)

```shell
raco pkg install sicp
```

then you can run:
```shell
racket ./test.rkt
```