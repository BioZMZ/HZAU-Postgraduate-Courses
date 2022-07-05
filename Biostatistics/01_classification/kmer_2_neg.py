from itertools import product
import os
import sys
import pandas
import numpy
def sample_formulation(seq,kmers):
    for i in range(len(kmers)):
        kmers[i] = ''.join(kmers[i])#将窗口碱基合并为字符串
    kmer_in_seq = []
    for i in range(len(seq)-1):#seq中kmer窗口个数
        kmer = seq[i:i+2]#seq分割为kmer序列
        kmer_in_seq.append(kmer)#将所有kmer存于列表中
    count = []
    for i in kmers:
        count.append(kmer_in_seq.count(i))#判断seq中的kmer是否在kmers中(0.1)
    feature_vector = [str(c/len(kmer_in_seq)) for c in count]
    return feature_vector
#计算每个kmer得分即判断每个kmer在每个kmers上的数目

base = 'ATCG'
kmers = list(product(base, repeat=2))#kmer是长度为4bases的所有可能
filename ="/beegfs/home/zmz/homework/01_data/classification/neg.fa"#输入fasta文件
with open(filename) as f:
    lines = f.readlines()
seq = []
for i in range(len(lines)):
    if i % 2 == 0:
        seq.append(lines[i+1].strip().upper())
#读取所有序列并存入列表
feature_vectors = []
for j in seq:
    feature_vectors.append(sample_formulation(j,kmers))


filename = filename.split('.')
feature_vectors = pandas.DataFrame(feature_vectors)#所有seq写成数据框
feature_vectors.to_csv("neg"+str(2)+"_"+'mer'+'.txt', index=False, header = False,encoding='gbk',float_format='%.4f',sep = " ")
