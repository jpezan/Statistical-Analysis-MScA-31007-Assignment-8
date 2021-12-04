# Statistical-Analysis-MScA-31007-Assignment-8
One Way ANOVA

## Assignment

#### Download your personal sample from left sidebar, unpack and read it:

dataPath <- "C:/path_to_your_dir/"
test_dat <- read.table(paste(dataPath,'Week8_Test_Sample.csv',sep = '/'), header=TRUE)

#### The sample test_dat has the following format:

test_dat$Output - output values;
test_dat$Treatment - predictor values;

#### Fit the data with models lm(Output ~ Treatment) and lm(Output~Treatement-1).

#### Calculate ANOVA tables necessary for answering questions of the quiz. Answer the questions.

#### You can also see in the interactive tab how between the groups variation and within the groups variation change when you change coefficients of the model.
