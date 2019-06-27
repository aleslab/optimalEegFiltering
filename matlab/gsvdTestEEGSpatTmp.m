load('../eegForwardData/fwdAllBrain.mat')
load('../eegForwardData/fwdOnlyVisual.mat')

nTrials = 300;
nElec = 128;
nTime = 400;
noiseSigma = .00001;
patNoiseSigma = 10;
signalE = fwdOnlyVisual(:,1:200:502)*1e-4;       
signalT = zeros(nTime,size(signalE,2));
t = linspace(-2*pi,2*pi,nTime);
%Create some random gabor signals
for iSignal = 1:size(signalE,2);
    
    thisPhase = 360*rand;
    thisFreq  = .75*rand+.05;
    thisSigma = abs(randn*2);
    signalT(:,iSignal)  = exp(-(t.^2)./(2*thisSigma.^2)).*cos(2*pi*thisFreq*(t+thisPhase));
    
end

signal=signalE*signalT';

noisePat1 = zeros(nElec,nTime,nTrials);
noisePat2 = zeros(nElec,nTime,nTrials);

noiseE  = fwdAllBrain(:,1:1000:end)*1e-4;
%noiseE = fwdOnlyVisual(:,10:100:602)*1e-4;       

for iTrial = 1:nTrials

noiseT  = randn(nTime,size(noiseE,2))*patNoiseSigma;
noisePat1(:,:,iTrial) = noiseE*noiseT';
noiseT  = randn(nTime,size(noiseE,2))*patNoiseSigma;
noisePat2(:,:,iTrial) = noiseE*noiseT';

end

pureSignal = repmat(signal,1,1,nTrials);
noise=randn(size(pureSignal))*noiseSigma;
pureNoise  = noise+noisePat1;
signalAll = pureSignal+pureNoise;

noiseAll = randn(size(signalAll))*noiseSigma +noisePat2;

[U,V,X,C,S] = gsvd(signalAll(:,:)',noiseAll(:,:)',0);

%         A = U*C*X'
%         B = V*S*X'
%         C'*C + S'*S = I 
%


%%
mseFiltList = [];
sfo = [];
uList = 1;linspace(.01,1,10);
for iU =1 :length(uList),
    thisU = uList(iU);
        
u=thisU;
%snrScale = (diag(C,0).^2-diag(S,0).^2)./(diag(C,0).^2+(u-1)*diag(S,0).^2);
%snrScale = (diag(C,0).^2-diag(S,0).^2)./(diag(S,0).^2);

p = size(signalAll(:,:),2);
m = size(noiseAll(:,:),2);
snrScale=(1-(p*diag(S,0).^2)./(m*diag(C,0).^2));
%snrScale= max(snrScale,0); %Noise levels below 0 mean 0. 

negSnr= ((diag(C,0).^2-diag(S,0).^2)<0);
snrScale(negSnr) = 0;

% [~,rSig] = qr(signalAll(:,:)',0);
% [~,rNoise] = qr(noiseAll(:,:)',0);
% [U,V,X,C,S] = gsvd(rSig,rNoise,0);

% %Assuming same size matrices.  Need to scale later.
% snrScale=(1-diag(S,0).^2./diag(C,0).^2);
% snrScaleQr = max(snrScale,0); %Noise levels below 0 mean 0. 


W = (pinv(X')*diag(snrScale)*X')';
%W = W/norm(W);

signalHat = W*signalAll(:,:);
signalHat = reshape(signalHat,size(signalAll));

meanSig = mean(signalAll,3);
meanSigHat = mean(signalHat,3);
meanNoise = mean(noiseAll,3);
%scaleFac = 1;
scaleFac = meanSig(:)'/meanSigHat(:)'
 scaleFacOracle = signal(:)'/ meanSigHat(:)'
sfo(iU) = scaleFacOracle;


signalHat = scaleFac*signalHat;
W = scaleFac*W;


%Two ways to make oracle filter.  Linear regress onto known basis 
%Or create the wiener filter 
%The oracle is the filter that noise the exact signal and noise pattern
% oracleFwd = [signalE noiseE];
% b=signalAll(:,:)'/oracleFwd';
% oracleRegress = oracleFwd(:,1:size(signalE,2))*b(:,1:size(signalE,2))';
% oracleRegress = reshape(oracleRegress,size(signalAll));

%oracleWeiner  = inv(Ryy)*Ryd 
%Y = measured data, d known signal. 
nNoiseSource = size(noiseE,2);
%Ryy = noiseSigma*eye(nElec)+ patNoiseSigma*(noiseE*noiseE') + signalE*signalE';
Ryy = noiseSigma*eye(nElec)+ patNoiseSigma*(noiseE*noiseE') + (pureSignal(:,:)*pureSignal(:,:)')/(600*200);

%Ryy = (pureNoise(:,:)*pureNoise(:,:)')/nTime*nTrials;
%Ryd = (pureSignal(:,:)*pureSignal(:,:)')/(600*200);
Ryd = (signalAll(:,:)*pureSignal(:,:)')/(nTime*nTrials);

wOracle = (pinv(Ryy)*Ryd)';
oracleFilt = reshape(wOracle*signalAll(:,:),size(signalAll));
meanOracle = mean(oracleFilt,3);
sf=(signal(:)'/meanOracle(:)');
oracleFilt = sf*oracleFilt;

% mseFilt = mean(mean(mean((pureSignal-signalHat).^2)))
% mseOracle = mean(mean(mean((pureSignal-oracleFilt).^2)))



disp('-------')
disp('Means:')
mseAve = mean(mean((signal-mean(signalAll,3)).^2))
mseFilt = mean(mean((signal-mean(signalHat,3)).^2))
mseFiltList(iU) = mseFilt;
mseOracle = mean(mean((signal-mean(oracleFilt,3)).^2))
%mseOracleRegress = mean(mean((signal-mean(oracleRegress,3)).^2))

oracleMSEList(iU) = mseOracle;

filtSig = W*mean(signal,3);
filtNoise = W*(mean(signalAll,3)-signal);
oracleFiltSig   = wOracle*mean(signal,3);
oracleFiltNoise = wOracle*(mean(signalAll,3)-signal);

distPow = mean(mean((filtSig-signal).^2))
noisePow =mean(mean((filtNoise).^2))

oracleDistPow = mean(mean((oracleFiltSig-signal).^2))
oracleNoisePow = mean(mean((oracleFiltNoise).^2))

disp('-------')

% %Check if mean before gsvd is better
[U,V,X,C,S] = gsvd(mean(signalAll,3)',mean(noiseAll,3)',0);

snrScale=(1-diag(S,0).^2./diag(C,0).^2);
snrScale= max(snrScale,0); %Noise levels below 0 mean 0. 

W = (inv(X')*diag(snrScale)*X')';

signalHat = W*mean(signalAll,3);

scaleFac = meanSig(:)'/signalHat(:)';
signalHat = scaleFac*signalHat;


signalSpatFilt = W*signalAll(:,:);
signalSpatFilt = reshape(signalSpatFilt,size(signalAll));
signalSpatFilt = shiftdim(signalSpatFilt,1);
noiseSpatFilt = W*noiseAll(:,:);
noiseSpatFilt = reshape(noiseSpatFilt,size(noiseAll));
noiseSpatFilt = shiftdim(noiseSpatFilt,1);

[sigSpatTempFilt, Wt, filterSNR, filterBasis] = gsvdFilter(mean(signalSpatFilt,2),mean(noiseSpatFilt,2));

mseMean = mean(mean((signal-signalHat).^2))

end



