# TetheringPredation

## Description
Data and scripts from the article "Suggestive Evidence of Differential Predation of Littorina saxatilis Color Morphs by Marbled Rock Crabs (Pachygrapsus marmoratus) As a Contributing Maintenance Mechanism To a Color Cline: Insights From a Field Tethering Experiment", by Juan Gefaell, Ramón Vigo, and Emilio Rolán-Alvarez.

## Contents
````
├──── Data
│ ├── Data_FreqDet.csv
│ ├── Data_PredExp.csv
│ ├── Data_RefSpec.csv
│ └── Data_ThickGeomMorph.csv
├──── Scripts
│ ├── Scripts_FreqDet.R
│ ├── Scripts_PredExp.R
│ ├── Scripts_RefSpec.R
│ └── Data_ThickGeomMorph.R
└── README.md
````

### Data\_FreqDet and Scripts\_FreqDet 
* This data and scripts correspond to the determination of frequencies of morphs and scars in Aguncheiro and Cabo Silleiro.

#### Variables in Data_FreqDet:

**Locality**: locality where the sampling for color morph and scars frequencies was carried out. 

**Sampling**: number of sampling replica.

**ColorMorph**: color morph of the snail; Albida (white), Lutea (yellow), Nigra (black), Lineata (with lines).

**Count**: Number of snails of each ColorMorph found in the Sampling.

**CountScars**: Number of snails of each ColorMorph within each Count that contained scars. Scars are defined as small breaks in the line of growth of the shell followed by new lines of growth. Breaks in the outer lip are only computed as scars if they are large and distinguishable, to avoid computing small breaks caused by the handling of the samples.

### Data\_PredExp and Scripts_PredExp 

* This data and scripts correspond to the tethering experiment (including analyses for Supplementary Material). 

#### Variables in Data_FreqDet:

**Session**: number of session of the tethering predation experiment.

	1=Session 1 (2022/09/27-2022/10/07)
	2=Session 2 (2022/10/11-2022/10/14)

**Transect**: number of transect.

**TransectPosition**: whether the transect is located in the lower to mid part or in the mid to upper part of the rocks. This is determined qualitatively, based on the overall position of the transect as seen from above in GoogleMaps and whether it has a barnacle coverage of nearly 100% or not.  
	
	Lower=Transect located in the lower to mid part of the rocks.
	Upper=Transect located in the mid to upper part of the rocks.

**Screw**: number of screw; it can be taken to be equal to the number of snail within each transect.

**SnailIndividual**: cumulative number of the snail used in the experiment, irrespective of the transect. 

**ColorMorph**: color morph of the snail; Lutea (yellow), Nigra (black), Lineata (with lines).

**Locality**: locality from which the snail comes; Aguncheiro (for Lutea, Nigra, and half of the Lineata), Silleiro (for the remaining half of Lineata). 

**ColorMorph_Locality**: concatenation between the variables ColorMorph and Locality. Allows for the distinction between Lineata from Aguncheiro and Silleiro.

**ShellLength**: shell length (mm) from apex to posterior end of body whorl, measured with a digital caliper to the nearest 0.1 mm.

**Outcome**: outcome of the snail involved in the experiment (based on Boulding et al. 2017).

	0=NA, data lost. We either did not tether any snail in this screw or did not retrieve the snail at EndDate. If the Outcome was very ambiguous or confusing we also ranked it 0=NA.
	1=AliveIntact. Alive snail with its shell undamaged and still attached to the tethering line.
	2=Chipped. Chipped; slightly chipped outer lip and snail alive. The outer lip of the shell aperture is visibly chipped or smoothly abrased, but the magnitude of the fracture is small, so cannot be readily attributed to a crab predation attempt. The difference between 'PreyedAlive' (=4) and 'Chipped' (=2) is the magnitude of shell breakage (PreyedAlive > Chipped): if the shell shows a sharp, abrupt fracture, it should be ranked as 4=PreyedAlive; if it shows a smooth or small break, then it should be ranked as 2=Chipped. 
	3=PreyedDead. Large or small fragments of shell attached to epoxy, which is still attached to the tethering line. No snail body within the shell. 
	4=PreyedAlive. Large fracture in the outer lip of the shell aperture, but the snail within still alive and attached to the tethering line. Much more dramatic shell break than in 'Chipped'. The difference between PreyedAlive (=4) and Chipped (=2) is the magnitude of shell breakage (PreyedAlive > Chipped): if the shell shows a clear sharp, abrupt fracture, it should be ranked as 4=PreyedAlive; if it shows a smooth, small break, then it should be ranked as 2=Chipped.
	5=EpoxyOnly. Only the epoxy is still attached to the tethering line and the snail is not found in the nearby surroundings. This is considered NA for most analyses, as it’s impossible to establish the real outcome of that snail.
	6=RecoveredAliveIntact. Tethering line and epoxy still attached to the screw ('EpoxyOnly'), and the snail found 'AliveIntact' (=1; i.e., undamaged) in the nearby surroundings. 
	7=RecoveredChipped. Tethering line and epoxy still attached to the screw ('EpoxyOnly'), and the snail found 'Chipped' (=2) in the nearby surroundings. 
	8=RecoveredPreyedDead. Tethering line and epoxy still attached to the screw ('EpoxyOnly'), and the snail found 'PreyedDead' (=3) in the nearby surroundings. 
	9=RecoveredPreyedAlive. Tethering line and epoxy still attached to the screw ('EpoxyOnly'), and the snail found 'PreyedAlive' (=4) in the nearby surroundings. 
	10=Hole. Dead snail with a hole in body whorl of shell likely from predation by Nucella.
	11=Lost. No line on the screw. Tethering line no longer attached to the screw, and the snail and line not found in the nearby surroundings. This is considered NA for most analyses, as it’s impossible to establish the real outcome of that snail.
	12=KnotOnly. Tethering line with no glue on the knot. Missing snail with the knot or tethering line visible.
	13=DeadIntact. Dead snail attached to the tethering line but with shell undamaged. 
	14=EmptyShell. Empty undamaged shell. Likely preyed by an organism that somehow manages to remove the animal from the shell without breaking it.
	NA=Missing data or highly ambiguous outcome.
 
**OutcomeNames**: variable showing the names of the Outcome instead of their corresponding numbers.

**Alive**: dummy variable that reflects whether the outcome of the snail was remaining alive, irrespective of whether it was attached to the tether or not (AliveIntact or RecoveredAliveIntact). It was created as =IF(OR(OutcomeNames=“ AliveIntact”, OutcomeNames=“ RecoveredAliveIntact”), 1, 0).  EpoxyOnly and Lost coded as =NA (because it’s impossible to establish the real outcome of these two cases; i.e., whether they are alive or not). OutcomeNames=NA were also taken as NA.

	0=Snail not alive.
	1=Snail alive

**Preyed**: dummy variable that reflects whether the outcome of the snail was predation by a crab or not (PreyedDead or PreyedAlive). It was created as =IF(OR(OutcomeNames=“PreyedDead”, OutcomeNames=“PreyedAlive”), 1, 0). EpoxyOnly and Lost coded as =NA (because it’s impossible to establish the real outcome of these two cases when it comes to predation). OutcomeNames=NA were also taken as NA.

	0=Snail not preyed by a crab.
	1=Snail preyed by a crab.

**Chipped**: dummy variable that reflects whether the outcome of the snail was Chipped (=2) or not. It was created as =IF(OR(OutcomeNames=“Chipped”), 1, 0). EpoxyOnly and Lost coded as =NA (because it's impossible to establish the real outcome of these two cases when it comes to predation). OutcomeNames=NA were also taken as NA.

	0=Snail not chipped.
	1=Snail chipped.

**PreyedChipped**: dummy variable created assuming the hypothesis that the outcome Chipped (=2) constitutes an instance of predation by a (small) crab, together with PreyedDead (=3) and PreyedAlive (=4). So can be defined as whether the outcome of the snail was predation by a crab (broadly construed) or not. It was created as =IF(OR(OutcomeNames=“PreyedDead”, OutcomeNames=“PreyedAlive”, OutcomeNames=“Chipped”), 1, 0). EpoxyOnly and Lost coded as =NA (because it's impossible to establish the real outcome of these two cases when it comes to predation). OutcomeNames=NA were also taken as NA.

	0=Snail not preyed by a crab (assuming Chipped as an instance of predation).
	1=Snail preyed by a crab (assuming Chipped as an instance of predation).

**EmptyShell**: dummy variable that reflects whether the outcome of the snail was EmptyShell (=14) or not. Aimed at testing the hypothesis of whether this constitutes an instance of predation by fish. It was created as =IF(OR(OutcomeNames=“EmptyShell”),1, 0). OutcomeNames=NA were also taken as NA.

	0=no EmptyShell outcome.
	1=EmptyShell outcome.
	
**EpoxyOnlyLost**: dummy variable grouping all cases of EpoxyOnly (=5), and Lost (=11) to test whether the position of the Transect somehow influences the loss of snails. It was created as =IF(OR(OutcomeNames=“EpoxyOnly”, OutcomeNames=“Lost”), 1, 0). OutcomeNames=NA were also taken as NA.

	0=Snail not lost.
	1=Snail lost.

### Data\_RefSpec and Scripts_RefSpec 

* This data and scripts correspond to measurements of reflectance spectrometry in a sample of rocks and snails from Aguncheiro and Silleiro (same snails as in ThickGeomMorph). 

#### Variables in Data_RefSpec:

**wl**: wavelength to which the different reflectances correspond.

* The next variables in the dataset correspond to the reflectance measures for each wavelength in the sample of snails and rocks. The names of the columns follow the following naming criteria:

**ColorMorphLocality\_Individual** (for snails). Example: LineataSilleiro\_1, LineataSilleiro\_2, etc.

**RockLocality\_IndividualMeasure** (for rocks). Example: RockSilleiro\_1, RockAguncheiro\_4. The first three measures of each rock correspond to rock nº 1, and the three last to rock nº 2.

### Data\_ThickGeomMorph and Scripts_ThickGeomMorph 

* This data and scripts correspond to measurements of thickness and geometric morphometrics in a sample of snails from Aguncheiro and Silleiro (same snails as in RefSpec)

#### Variables in Data_ThickGeomMorph:

**ColorMorph**: color morph of the snail; Lutea (yellow), Nigra (black), Lineata (with lines).
ColorMorphNum: number assigned to each color morph.
 
	1=Lutea.
	2=Nigra.
	3=Lineata_Aguncheiro.
	4=Lineata_Silleiro.
	
**Locality**: locality from which the snail comes; Aguncheiro (for Lutea, Nigra, and half of the Lineata), Silleiro (for the remaining half of Lineata). 

**ColorMorph\_Locality**: concatenation between the variables ColorMorph and Locality. Allows for distinction between Lineata from Aguncheiro and Silleiro.

**Individual**: number of snail within each ColorMorph_Locality.

**ShellThickness**: shell thickness of the last whorl of the snails, measured in mm.
 
**CS**: centroid size; the square root of the sum of squared distances of the different landmarks from their centroid. 

**RW1**: relative warp 1, explaining the most variance.

…

**RW18**: relative warp 18, explaining the least variance. 


