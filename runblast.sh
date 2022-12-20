<<com
Parses all fasta files and compares it to a curated proteome database and ouputs blastp query files. 

	Parameters
	----------
	evalue: significance threshold of 1e-8
	outfmt: 6 = tabular format for outputs
com
for i in *faa
do
blastp -query $i -db /Users/elvisyung/Desktop/t1-research/genome_bank/proteome-all-db  -evalue 1e-8 -outfmt 6 -out ${i}_out.csv
done
