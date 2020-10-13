# spartina.genetics

These are the associated files and R code for the SNP dataset that are a part of ***Microgeographic genetic divergence within a coastal foundation species contributes to ecosystem function*** Collaboration with Robyn Zerebecki, Randall Hughes, Torrance Hanley, Chris Nice and others.

## data

1) ***inds/***  
Description: a set of files with individuals used in the analyses.  
a. ***allpops_individualIDs_subset.csv*** - set of 309 individuals - all sites.  
b. ***indIDS.T.S.POPS.txt*** - set of six marshes with tall-vs-short comparisons.  
c. ***inds.FL*** ***inds.RISRIT*** ***inds.SBITBI*** ***inds.SFBTFB*** ***inds.SWSSWT*** ***inds.WESWET*** - six marshes with tall-vs-short comparisons  

2) ***loci.subset2.txt***  
Description: names of loci used in this study  

3) ***Spartina_SNP_SiteID.csv***  
Description: meta-data for each location.  

4) ***spartinaNov2017.called.subset.mpgl***  
Description: set of genotype likelihoods for all individuals. values range from 0-2; We converted the phred-scale genotype likelihoods (from samtools/bcftools) per SNP-sample combination into probabilities that summed to 1, and then converted these to a single value that ranges from 0 to 2, where 0, 1 and 2 represent the highest probability of a homozygote, heterozygote, and alternative homozygote, respectively. 1st column = location; columns 2-310: 309 individuals. 2735 rows = 2735 loci. 

5) ***t.s.pops.beagle.gz 309inds.beagle.gz***
Description: genotype likelihoods (from samtools/bcftools) in beagle-formatted file generated by angsd. Each individual X SNP combination occurs across 3 columns (AA, AB, BB).

6) ***fst/***  
Description: fst output from angsd.  

7) ***admix.runs.TvS.POPS/*** ***admix.runs.ALLPOPS***  
Description: NGSadmix runs for all populations and tall-vs-short comparisons.

8) ***allelefreq***  
Description: these were generated by the "angsd -doMaf 4" protocol on site-specific beagle-formatted files.  

9) ***Hobs***  
Description: est.ml files from the "angsd -dosaf 1 -gl 1". "For diploid single samples the heterozygosity is simply second value in the SFS/AFS" see http://www.popgen.dk/angsd/index.php/Heterozygosity  

10) ***rda/***  
Description: beagle file + list of individuals + list of loci for each of the 6 marshes (tall vs short)  

11) ***misc/phenotypeFigs.zip***  
Description: Experiment and greenhouse dataset to make figures in main text.  

### Figure: PCAs  
Based on genotype likelihoods of individuals embedded in the .mpgl file.  Uses the prcomp() to generate PCAs. there are four versions:  
* ***R/map of spartina sites.R*** and ***output/map of spartina sites.pdf*** - this includes all sites, and is coupled with a map of the east coast of the US.  
* ***R/shortVStall-pca-allDATA.R*** and ***shortVStall-pca-allDATA.pdf*** - this compares PCAs of tall-vs-short individuals within each of 6 marshes.     
* ***R/shortVStall-pca_wout10.R*** and ***shortVStall-pca_wout10.pdf*** - same as previous, but side-by-side compares when all data or the 10% outlier loci (by Fst) are removed.  
* ***R/shortVStall-pca-&admix_allData.R*** and ***shortVStall-pca&admix_allDATA.pdf*** - This does a side-by-side comparison of admixture plots and PCA for 6 marshes.    

### Figure: admixture plots  
Based on NGSadmix analyses in angsd.  
* ***R/ngsAdmix.pretty.R*** and ***output/ngsAdmix.pretty.pdf***  - all sites and 309 individuals  
* ***R/ngsAdmix.T.S.POPS.pretty.R*** and ***output/ngsAdmix.T.S.POPS.pretty.pdf*** - only the 6 marshes in which tall-vs-short occurs.   

### Figure: Lnl and deltaK of admixture plots  
Based on 5 independent runs of NGSadmix in angsd  
* ***R/deltaK.R*** and ***output/deltaK.pdf***  

### Figure: Fst heatmap
These are based on output from ngsFST. ***R/ProcessFstFiles.R*** takes the pairwise files and creates estimates of mean Fst and geographic distance using library(geosphere) - placed into ***output/fst.siteXpop.csv***. This is used to create a heat-map.  
* ***R/fst-heat-map-spartina.R*** and ***output/fst-heat-map-spartina.pdf***  

### Figure: other Fst plots  
These are based on direct output from ngsFST.  
* ***R/FstViolin.R*** and ***output/FstViolin.pdf*** and ***output/FstMeans.pdf***  
* ***R/Fst.morePlots.pdf*** and ***output/Fst.morePlots.pdf***  

### Figure: Hobs  
THese are analyses of observed heterozygosity.  
* ***R/processHobs.R*** and ***output/Hobs.csv*** and ***output/Hobs~lat.pdf***  

### Figure: Hexp  
These are analyses of expected heterozygosity, taking the allele frequencies as input    
* ***R/HexpCalculation.R*** and ***output/Hexp.csv*** and ***output/Hexp~lat.pdf***  

### Figure: RDA  
These are analyses of RDA (basically PCA) and loci associated with TvS.  
* ***R/rda.R*** and ***output/rda.pdf***: figure of x=RDA (tall vs short) and y=PC1.  
* ***output/candidateLoci.Rda***: a list of loci that extend 2.5 SD away  
* ***output/candidateLoci.summary.txt***: a table that summarizes overlap between candidate loci among 6 populuations (loci outside of SD = 2.5)  
* ***output/NOTcandidateLoci.summary.txt***: a table that summarizes overlap between loci that were NOT outliers.  

