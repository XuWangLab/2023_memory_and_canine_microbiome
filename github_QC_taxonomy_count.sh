#QC: Remove adapter + trim low quality bases
adapter=/dir/ref_genomes/NEB_dual.fa

java -jar /dir/tools/Trimmomatic-0.36/trimmomatic-0.36.jar SE -threads 7 -phred33 /dir/file/${FqNAME}.fq.gz /dir/file/${FqNAME}_r1_trim.fq ILLUMINACLIP:$adapter:2:30:10 LEADING:3 TRAILING:3 SLIDINGWINDOW:4:15 MINLEN:36

#remove host 
module load bwa
module load samtools

bwa mem -t 7 /dir/ref_genomes/dog_genome_31.fa /dir/file/${FqNAME}_r1_trim.fq > /dir/file/${FqNAME}_WGSmeta_host.sam

samtools fastq -@ 7 -f 4 /dir/file/${FqNAME}_WGSmeta_host.sam > /dir/file/${FqNAME}_WGSmeta_unhost.fq

#taxonomy counts
bwa mem -t 7 /dir/ref_genomes/dog_gut_microbiome_reference_contigs.fa /dir/file/${FqNAME}_r1_trim.fq > /dir/file/${FqNAME}_WGSmeta_contig.sam

samtools view -@ 7 -bS -F 4 /dir/file/${FqNAME}_WGSmeta_contig.sam > /dir/file/${FqNAME}_WGSmeta_contig_unsorted.bam

module load bedtools

bedtools coverage -a location.txt -b /dir/file/${FqNAME}_WGSmeta_contig_unsorted.bam -counts > /dir/file/${FqNAME}_taxonomy_count.txt