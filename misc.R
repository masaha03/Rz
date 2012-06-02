library(methods)
library(RGtk2)
library(memisc)
setwd("/Users/masahiro/Dropbox/Documents/R/Rz/Rz/R/")
setwd("/media/sf_Dropbox/Documents/R/Rz/Rz/R/")
sapply(dir(), source)
Rz()
#setwd("../../")

q(save="no")
library(RGtk2);library(Rz);Rz();Rz()


#性別 <- factor(rbinom(1:1000, 1, 0.5), labels=c("男性", "女性"))
#年齢 <- factor(rbinom(1:1000, 2, 0.4), labels=c("若年層", "中年層", "高年層"))
#身長 <- factor(rbinom(1:1000, 2, 0.6), labels=c("低", "中", "高"))
#t1 <- crossTable(性別, 年齢, 身長)
#summary(t1, latex=FALSE)

bindtextdomain("Rz", "/home/masahiro/Documents/R/Rz/Rz/inst/po")
library(tools)
xgettext2pot("/home/masahiro/Documents/R/Rz/Rz",
             "/home/masahiro/Documents/R/Rz/Rz/inst/po/R-Rz.pot")
xgettext2pot("~/Dropbox/Documents/R/Rz/Rz",
             "~/Dropbox/Documents/R/Rz/Rz/inst/po/R-Rz.pot")
column.definition <- c(index=0, select=1, vars=2, var.labs=3, msr=4, msr.image=5, val.labs=6, missing=7)

#R CMD check Rz --no-manual --no-install
#R CMD build Rz --binary

### todo
# log
# ラベル等をファイルから読み込み
# スクリプトインターフェース
# レポート作成
# 変数の計算
# ポップアップ改善
# 変数にタグ
# クロス集計表
# ワーディング管理
# ケースの選択
# 最近使ったファイル
# unit of plot file.
# 保存のときのrzdのエンコーディング
# プロットウィンドウ

# 画像のフォント
# 入力がスペースだけの場合

