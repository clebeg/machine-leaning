#File-Name:       simple_decryption.R
#Date:            2015-01-07 10:14:00
#Author:          Clebeg
#Email:           clebeg@163.com
#Purpose:         Code for study "机器学习：实用案例解析（R语言） 优化：密码破译"
#Data Used:       维基百科上每个单词出现的频率（非常有用的数据）lexical_database.Rdata
#Packages Used:   ggplot2
#Machine:         Clebeg'Lenovo Y485

# All source code is copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/
# All rights reserved.

#auto create letter a to z, 'letter' is the inner object meas a-z

#这个案例实现了Metropolis算法，但是Metropolis算法在一定概率上会放弃好的结果
#为了避免这种事情发生，可以选择模拟退火算法，这种算法会随着运行次数的增加
#放弃好算法的概率会变得越来越小，最后会稳定很多。
#换句话说就是会随着运行次数的增加，变得越来越贪心。
english.letters <- letters

#恺撒密码
caesar.cipher <- list()

inverse.caeser.cipher <- list()

#恺撒密码 加密以及解密
for(i in 1:length(english.letters)) {
  caesar.cipher[[english.letters[i]]] <- english.letters[i%%length(english.letters) + 1]
  inverse.caeser.cipher[[english.letters[i%%length(english.letters) + 1]]] <- english.letters[i]
}

#Function-Name:WordCipher
#Target:                将单词按照 cipher 规则加密
#Param1:word            need cipher word
#Param2:cipher          cipher by caeser cipher algro
#Return1:CipherText     the text ciphered by the cipher
WordCipher <- function(word, cipher) {
  output <- ''
  for (i in 1:nchar(word)) {
    output <- paste0(output, cipher[[substring(word, i, i)]])
  }
  return(output)
}

TextCipher <- function(text, cipher) {
  output <- c()
  for (word in text) {
    output <- c(output, WordCipher(word, cipher))
  }
  return(output)
}

TextCipher(c('tbnqmf', "ufyu"), caesar.cipher)

#Function-Name:RandomInitCipher
#Target：               随机初始化加密规则
#Return1:cipher         加密规则
RandomInitCipher <- function() {
  cipher <- list()
  cipher.inputs <- letters
  cipher.outs <- letters[sample(1:length(letters), length(letters))]
  for (index in 1:length(letters)) {
    cipher[[cipher.inputs[index]]] <- cipher.outs[index]
  }
  return(cipher)
}

#Function-Name:ModifyCipher
#Target：               随机改变加密规则中的某个字母的对应规则，得到新的规则
#Param1:cipher          原来的加密规则
#Param2:input           需要加密的字母input
#Param3:output          需要加密的字母加密为output
#Return1:new.cipher         新加密规则
ModifyCipher <- function(cipher, input, output) {
  new.cipher <- cipher
  #记录需要改变加密规则的字母原始的加密字母
  cipher.old.output <- cipher[[input]]
  
  #记录新被使用来加密的字母原始对应字母
  cipher.old.input <- names(which(
    #sapply 用法，循环第一个参数的每一个原始，用后面的匿名函数处理，返回list
    sapply(names(cipher), function(key) {cipher[[key]]}) == output
    ))
  
  #交换对应关系
  new.cipher[[input]] <- output
  new.cipher[[cipher.old.input]] <- cipher.old.output
  return(new.cipher)
}

RandomModifyCipher <- function(cipher) {
  input <- sample(names(cipher), 1)
  output <- sample(letters, 1)
  ModifyCipher(cipher, input, output)
}


#下面的数据是维基百科上出现的每个单词的频率
load("decryption_algorithm/lexical_database.Rdata")

#Function-Name:WordProb
#Target：               利用维基百科的数据库，计算一个单词出现的概率
#Param1:lexical.database          
#Param2:word            需要计算概率的单词
#Return1:probability    单词出现的概率，如果从未出现，则概率设置为.Machine$double.eps
WordProb <- function(word, lexical.database) {
  prob <- lexical.database[[word]]
  
  if (is.null(prob) || is.na(prob) || prob == 0) {
    return(.Machine$double.eps)
  }
  return(prob)
}


#Function-Name:TextProb
#Target：               计算一个文本串出现的概率，每个单词出现的概率的积，然后取对数，
#实际上就是没有单词出现的概率取对数之后的和
#Param1:lexical.database          
#Param2:text            需要计算概率的字符串
#Param3:cipher          加密规则
#Return1:probability    字符串出现的概率
TextProb <- function(text, cipher, lexical.database) {
  log.prob <- 0
  for (word in text) {
    decipher.word <- WordCipher(word, cipher)
    log.prob <- log.prob + log(WordProb(decipher.word, lexical.database))
  }
  return(log.prob)
}

#Function-Name:MetroPolis.step
#Target：               motropolis单步操作过程，对于新的加密规则是否采用
#Param1:lexical.database          
#Param2:text            原始字符串
#Param3:cipher          原始加密规则
#Return1:cipher         加密规则
MetroPolis.step <- function(text, cipher, lexical.database) {
  new.cipher <- RandomModifyCipher(cipher)
  old.pro <- TextProb(text, cipher, lexical.database)
  new.pro <- TextProb(text, cipher, lexical.database)
  
  if (new.pro > old.pro) {
    return(new.cipher)
  } else {
    a <- exp(new.pro - old.pro)
    p <- runif(1)
    if (p < a) {
      return(new.cipher)
    } else {
      return(cipher)
    }
  }
}


decrypted.text <- c('here', 'is', 'some', 'sample', 'text')
encrypted.text <- TextCipher(decrypted.text, caesar.cipher)

set.seed(1)
cipher <- RandomInitCipher()
results <- data.frame()

number.of.iterations <- 50000

for (it in 1:number.of.iterations) {
  
  log.pro <- TextProb(encrypted.text, cipher, lexical.database)
  current.decryted.text <- paste(TextCipher(encrypted.text, cipher), collapse = ' ')
  correct.text <- paste(decrypted.text, collapse = ' ')
  results <- rbind(results,
      data.frame(
          Iteration = it,
          LogProbability = log.pro,
          CurrentDecryptedText <- current.decryted.text,
          CorrectText <- correct.text
        )
    )
  
  cipher <- MetroPolis.step(encrypted.text, cipher, lexical.database)

}

write.csv(results, file = 'decryption_algorithm/results.csv', row.names = FALSE)








