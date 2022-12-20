from Bio import SeqIO
import pandas as pd
from collections import defaultdict

"""
Parses downloaded Genome files from the US National Center for Biotechnology Information's GenBank website
(https://www.ncbi.nlm.nih.gov/data-hub/genome; database: Protein) and creates individual fasta files by 
extracting sequence id and sequences from the list of speices indicated in Table Supplementary 1 from Supplmentary_Material. 
"""
filenames_excel = pd.read_csv('file_name.csv')

for i in range(len(filenames_excel.index)):
    seqs = [(str(seq.id), str(seq.seq)) for seq in SeqIO.parse(filenames_excel.file_name[i] + ".faa",'fasta')]

    d = defaultdict(list)

    for header,sequence in seqs:
            d[header].append(sequence)
    dict_seq = {}
    for header, sequence in d.items():
        dict_seq[header] = '\n'.join(sequence)

    for header, sequence in dict_seq.items():
        with open('/user/work/kt20672/' + filenames_excel.file_name[i] + '/' + header + '.faa', 'w') as f:
            for seq in sequence:
                f.write(seq)
