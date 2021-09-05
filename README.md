Rによるデータ解析のためのデータ可視化
================

2021年度 統計関連学会連合大会 チュートリアル
「Rによるデータ解析のためのデータ可視化」、第三部「地図を描画する」の資料置き場です。

- [スライド](https://rawcdn.githack.com/uribo/jfssa2021_datavis/main/slide/210905_jfssa_dataviz_tutorial.pdf)
- [コード](https://github.com/uribo/jfssa2021_datavis/blob/main/part3.Rmd)


## セットアップ

```r
usethis::use_course("uribo/jfssa2021_datavis")
```

をR上で実行すると、このリポジトリの内容がダウンロードされ、
チュートリアルで取り上げるコードやデータが揃います。

いくつかのRパッケージを利用します。次のコマンドを実行して
インストールを行ってください。

```r
install.packages(c("tidyverse", "sf", "rnaturalearth", "tabularmaps"))
```

## 実行環境

- R 4.0.5
