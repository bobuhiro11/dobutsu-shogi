# dobutsu-shogi

This game is based on the complete analysis of Animal Shogi ([どうぶつしょうぎ](https://ja.wikipedia.org/wiki/%E3%81%A9%E3%81%86%E3%81%B6%E3%81%A4%E3%81%97%E3%82%87%E3%81%86%E3%81%8E)), and the CPU plays its best moves based on the complete analysis.

## How to use

```bash
curl -LO https://github.com/bobuhiro11/dobutsu-shogi/releases/download/0.3.0/dobutsu-shogi-0.3.0-SNAPSHOT-standalone.jar
curl -LO https://github.com/bobuhiro11/dbtshougi-analysis/releases/download/v0.0.1/all-state_sorted.dat
curl -LO https://github.com/bobuhiro11/dbtshougi-analysis/releases/download/v0.0.1/next_state.dat
java -jar dobutsu-shogi-0.3.0-SNAPSHOT-standalone.jar
```

## How to build

```bash
lein uberjar
java -jar target/dobutsu-shogi-0.3.0-SNAPSHOT-standalone.jar
```

![](https://github.com/bobuhiro11/dobutsu-shogi/releases/download/0.3.0/screenshot.png)
