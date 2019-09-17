# Statistical Inference Shiny Apps
This repository contains shiny apps for teaching and learning introductory statistics concepts. Each app includes an
"Interface" tab to be explored as well as an "Info" tab that provides context and things to notice and try in the app.

The apps are designed to develop intuition about core concepts in statistical inference, including standard deviation, 
z-scores, and confidence intervals as well as key properties of the normal distribution and standard normal distribution. 

The apps will help learners explore- and even construct for themselves- properties of the normal distribution that 
play a critical role in determining what constitutes valid and convincing statistical evidence. 
Much of scientific inquiry relies on statistical inference, and nearly every person has consumed “evidence” that arose 
from statistical inference, whether they were aware of it or not. Despite being ubiquitous in modern society, 
the mechanisms behind statistical inference are notoriously abstract and confound students and users of statistics 
everywhere. The widespread and persistent lack of sound statistical reasoning is well-documented and researched 
(Garfield 2002; Garfield and Ahlgren 1988; Tversky and Kahneman 1974). 

Statistics courses often focus on formulas and procedures, but even when students “master” those procedures, sound 
statistical reasoning does not necessarily, or even commonly, follow (Garfield 2002). This emphasis on “following a 
prescribed procedure rather than reasoning from the evidence gathered in the procedure” is a common problem within 
traditional classrooms, for which constructionist learning environments offer an alternative (Wilensky and Reisman 2006).  
Typically, z-scores and their use in confidence intervals and hypothesis tests are taken to be some abstract and magical 
process. People are not really sure why z-scores allow us to make claims about when an observation is unusual or how that 
constitutes evidence; people just blindly trust that the procedure they are told to follow results in “statistically 
significant” evidence, and consumers of that evidence accept those claims as truth. These constructionist apps are designed 
to make the mechanisms behind statistical more salient, so that understanding and intuition precedes and informs procedures.

## AUDIENCE AND IDEAL CONTEXT
The examples in the apps were chosen to be relevant to college students, and the accompanying curricular materials 
in the "Info" tabs of the apps are written for the context of an undergraduate introductory 
statistics course. However, the material would be suitable for an introductory statistics course at the high school level 
as well. 

Note that these apps could be used as a stand-alone series to orient new students to statistical inference, 
but they are intended to supplement material typically covered in an introductory course. As a whole, these apps are 
especially useful in developing the intuition needed to conduct hypothesis tests and therefore should precede a 
hypothesis test unit. 

Ideally, the activities would be done in class or a lab section (time permitting) so that students can discuss findings 
with one another and the professor, especially as they reason about some of the more complex ideas. However, the activities 
could be assigned as group projects to be done outside of class. The activities are structured in such a way that it would 
be natural for students to type up a brief report of their findings if assigned this way.

## LEARNING GOALS 
The student should walk away with an intuitive understanding of standard deviation, z-scores, the empirical rule, and 
confidence intervals. They will develop the notion of standard deviation as a measure of "spread." They will construct 
the "empirical rule" for themselves, which will lead to an understanding of how standard deviation can be used as a way to 
judge which observations are extreme or unusual. They will develop an understanding of z-scores as a standardized scale for 
measuring distance from the mean in "standard deviation units." They will discover for themselves which z-scores and 
critical values should be used as thresholds for classifying extreme or unusual observations. They will use their new 
knowledge of standard deviations and z-scores to develop an intuitive understanding of confidence intervals. They should be 
able to explain what is meant when we say we are statistically "95% confident" about something as well as what the 
relationship is between the width of an interval, the level of confidence, and the sample size, and what relevance those 
three things have when they are trying to make a statistical decision about evidence.

The overarching goal is to allow students to construct for themselves important properties of the normal distribution 
that are not always salient or intuitive in traditional statistics instruction. The goal is that these apps will lay the 
foundation for a more intuitive understanding of what constitutes convincing evidence that an observation is extreme or 
unusual for any given distribution. Once students have seen over and over again with many different generated datasets, 
that 95% of observations fall within approximately 2 standard deviations of the mean, they will understand why observations 
more than two standard deviations away could be considered unusual. Although a app for hypothesis tests is not currently 
included in this repository, it is a natural extension of all of the ideas covered in these apps. 

The empirical rule and z-scores are extremely important and powerful characteristics of the normal distribution that 
enable scientists to make decisions about whether data provides convincing evidence, but deriving them requires 
sophisticated calculus and probability theory. Thus, they are typically presented to students as facts to memorize and 
remain abstract and arbitrary in the mind of the student. These properties emerge through simulations with thousands of 
iterations, however, which powerfully enables students to observe and construct these properties for themselves. 
These three apps will give students the framework and intuition they need for understanding statistical inference and for 
deciding when data provides convincing evidence about a claim. 

The specific learning goals and the ideas and properties that should become more salient in each of the three apps are 
given below.  

### Standard Deviation app
1.	Standard deviation is a measure of how “spread out” the data is
2.	The Empirical Rule (general property of the normal distribution)
    + Approximately 68.5% of observations fall within 1 sd of the mean
    + Approximately 95% of observations fall within 2 sds of the mean
    + Approximately 99.7% of observations fall within 3 sds of the mean
3.	The normal distribution is symmetric and can be described in terms of its mean and standard deviation

### Z-Scores app
1.	Z-scores are a measure of the distance of an observation from its mean, in terms of standard deviation units
    + Negative z-scores correspond to x-values below the mean
    + A z-score of 0 corresponds to x-values equal to the mean
    + Positive z-scores correspond to x-values above the mean
2.	Z-scores are scale-invariant by design, because they scale observations by their mean and standard deviation
3.	The Empirical Rule (general property of the normal distribution)
    + Approximately 68.5% of observations fall within 1 sd of the mean
    + Approximately 95% of observations fall within 2 sds of the mean
    + Approximately 99.7% of observations fall within 3 sds of the mean
4.	The normal distribution is symmetric and can be described in terms of its mean and standard deviation
5.	There is a one-to-one relationship between x-values and z-scores
6.	Therefore, every normal distribution can be scaled to a special case called the standard normal distribution, which has mean 0 and standard deviation 1, by converting all observations to their z-scores

### Confidence Intervals app
1.	Drawing a random sample inherently results in “noise” and uncertainty when trying to estimate a true population mean. A confidence interval is a way to account for this uncertainty.
2.	Confidence intervals are another example of creating a range around a mean in terms of standard deviation units.
3.	The level of confidence refers to the probability that the given procedure (with the specified sample size and z_star value) will result in an interval that contains the true mean. 
4.	Larger intervals correspond to higher levels of confidence and are obtained by using larger z_star cutoff values
5.	Narrower intervals imply more precise estimates, but the trade-off is lower levels of confidence when same sample size is held constant.
6.	Increasing sample size decreases the width of the interval (i.e. increases precision) but does not affect the level of confidence. 
7.	Confidence levels/success rates of 90%, 95%, and 99% correspond to z_star values of 1.645, 1.96, and 2.576. 
The 95%-1.96 relationship is a more precise estimate of the 95% and 2 standard deviation relationship determined in the 
empirical rule. 
