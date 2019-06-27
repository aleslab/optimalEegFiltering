%This script demonstrates the concepts behind using GSVD to compute linear
%subspace filters of data using a toy 2d problem.
%

%
%While many steps are straightfoward there are a few 
set(groot,'defaultaxesXAxisLocation','bottom')
set(groot,'defaultaxesYAxisLocation','left')
set(groot,'defaultaxeslinewidth',3)
set(groot,'defaultlinelinewidth',1)
set(groot,'defaultaxesfontsize',20)
set(groot,'defaultaxesbox','off');
%set(gca,'xaxislocation','origin')


%Define signal and noise covariance here:
%Let's define a signal and noise that have strong covariance structure 
%This will emphasize how the Wiener filtering works.  For it to work there
%needs to be some structure to exploit.  If the signal and noise are uncorrelated
%gaussian (e.g. white guassian) then there is nothing to learn.  
%By creating a covariance structures that correspond to signals and noise
%that are 1-d in a 2-d measurement space we can 
%
noiseCovariance = 1*([1 .2 ;.5 .2] + 2*eye(2)*eps); %Add an epsilon of identity matrix to ensure postive-definite for chol. 
%noiseCovariance = 1*([1 1 ;1 1] + 2*eye(2)*eps); %Add an epsilon of identity matrix to ensure postive-definite for chol. 

signalCovariance = 2*[2 0; 0 .001];

noiseR = chol(noiseCovariance);
signalR = chol(signalCovariance);
%noiseR = 2*([1 .5]'*[.707 .707])';
noiseR = 2*([1 .5; .1 .1]'*[1 0; 0 1])';


meanSigMag = 0;

nP = 1000;
signalData = signalR'*randn(2,nP) +[1 0; 0 -1]* ones(2,nP)*meanSigMag;
noiseData = noiseR'*randn(2,nP)+[-1 0; 0 1]* ones(2,nP)*meanSigMag;
measuredData = signalData+noiseData;
noiseSampleData = noiseR'*randn(2,nP)+[-1 0; 0 1]* ones(2,nP)*meanSigMag;

%The oracle
oracleW = signalCovariance/(signalCovariance+noiseCovariance)

figure(101);clf
%Show the signal and noise separately
h(1)= plot(noiseData(1,:),noiseData(2,:),'o');
hold on;
h(2)=plot(signalData(1,:),signalData(2,:),'o');

set(h(1),'markerfacecolor',h(1).Color,'markeredgecolor','k')
set(h(2),'markerfacecolor',h(2).Color,'markeredgecolor','k')
legend('Noise Only', 'Signal Only')
axis([-10 10 -10 10])
box off
xlabel('Sensor 1');ylabel( 'Sensor 2')

%measuredData2 = bsxfun(@minus,measuredData,mean(measuredData,2));
%measuredData2 = bsxfun(@minus,measuredData,mean(measuredData,2));
%noiseSampleData2 = bsxfun(@minus,noiseSampleData,mean(noiseSampleData,2));

uY = mean(measuredData,2);
uX = uY-mean(noiseSampleData,2);
measuredData2 = bsxfun(@minus,measuredData,uY);
noiseSampleData2 = bsxfun(@minus,noiseSampleData,mean(noiseSampleData,2));

[U,V,X,C,S] = gsvd(measuredData2',noiseSampleData2',0);

 u=1;
 snrScale = (diag(C,0).^2-diag(S,0).^2)./(diag(C,0).^2+(u-1)*diag(S,0).^2);
%snrScale = (diag(C,0).^2-diag(S,0).^2)./(diag(S,0).^2);

%snrScale=(1-diag(S,0).^2./diag(C,0).^2);
%snrScale= max(snrScale,0); %Noise levels below 0 mean 0. 

negSnr= ((diag(C,0).^2-diag(S,0).^2)<0);
snrScale(negSnr) = 0;

%W = (inv(X')*diag(snrScale)*X')';
gsvdW = ((X'\diag(snrScale) )*X')';

%Now lets also calculate using the Weiner formula with the identy, assuming
%Signal and noise are not-correlated:
%Rxy = Ryy - Rnn
%Ryy = signal plus noise, Rnn = noise sample, Rxy true signal and data
%covariance. 
%This is to demonstrate the connection between the GSVD algorithm and the
%Wiener filter.  However, this algorithm is not optimal to use in practice
%to compute weights because calculating covariance matrices and taking their inverse results
%in unstable weights when data high dimensional. 
Ryy = cov(measuredData');
Rnn = cov(noiseSampleData');
Rxy = Ryy-Rnn;
covW = Rxy/Ryy;

cleanedDataGsvd = bsxfun(@plus,gsvdW*measuredData,uX);
cleanedDataCov = covW*measuredData;
cleanedDataOracle = oracleW*measuredData;

mseRaw    = mean((signalData(:)-measuredData(:)).^2)
mseOracle = mean((signalData(:)-cleanedDataOracle(:)).^2)
mseGsvd   = mean((signalData(:)-cleanedDataGsvd(:)).^2)
mseCov    = mean((signalData(:)-cleanedDataCov(:)).^2)
l1 = 10*X(:,1) ./max(X(:,1));
l2 = 10*X(:,2) ./max(X(:,2));

figure(102);clf;hold on

xD = [measuredData(1,:); cleanedDataGsvd(1,:)];
yD = [measuredData(2,:); cleanedDataGsvd(2,:)];

plot(xD,yD,'color',[.5 .5 .5],'HandleVisibility','off');

plot([-l1(1) l1(1)],[-l1(2) l1(2)],'linewidth',3,'color',[0.9290    0.6940    0.1250])
plot([-l2(1) l2(1)],[-l2(2) l2(2)],'linewidth',3,'color',[ 0.8500    0.3250    0.0980] )

%Show the signal and noise separately
h(1)= plot(measuredData(1,:),measuredData(2,:),'o','color',[0.4940 0.1840 0.5560]);
hold on;

set(h(1),'markerfacecolor',h(1).Color,'markeredgecolor','k')


axis([-10 10 -10 10])
h(1)= plot(cleanedDataGsvd(1,:),cleanedDataGsvd(2,:),'o','color',[0.2 0.4 0.5560]);

set(h(1),'markerfacecolor',h(1).Color,'markeredgecolor','k')

legend('Noise Component','Signal Component','Measured Data','Clean Data')
box off
xlabel('Sensor 1');ylabel( 'Sensor 2')




figure(103);clf
%h(1)=plot(signalData(1,:),signalData(2,:),'o');
hold on;
h(1)= plot(cleanedDataGsvd(1,:),cleanedDataGsvd(2,:),'o','color',[0.2 0.4 0.5560]);

set(h(1),'markerfacecolor',h(1).Color,'markeredgecolor','k')
box off
xlabel('Sensor 1');ylabel( 'Sensor 2')

axis([-10 10 -10 10])


