function [filterWeights, filteredSignal, filteredNoise, filterProperties ] = gsvdFilter(signal,noise,method)
%
%
%
signal2d = signal(:,:)';
noise2d  = noise(:,:)';

if nargin()<3;
    method = 'mse';
end
%First perform an SVD to remove singular dimensions
%Singular dimensions are often present in real data due to
%pre-processing (e.g. removing channels, referencing);
[u s v] = svd([signal2d' noise2d'],'econ');

%WARNING WARNING- Magic number here. 
comps2keep = diag(s)./sum(diag(s))>1e-6;

%Find the maximum dimension to keep. We are constrained to stay in the
%no larger than the smallest fo these 3.  
maxComp = min([size(comps2keep,1) size(signal2d,1) size(noise2d,1)]);

uFilter = u(:,comps2keep(1:maxComp));
svdNumCompsUsed = (sum(comps2keep(1:maxComp)));

[U,V,filterBasis,C,S] = gsvd(signal2d*uFilter,noise2d*uFilter,0);



%Add distortion factor changes. 
% u=1;
% snrScale = (diag(C,0).^2-diag(S,0).^2)./(diag(C,0).^2+(u-1)*diag(S,0).^2);

%Doclo et al. 2002:
p = size(signal2d,1);
q = size(noise2d,1);

if strcmpi(method,'mse')
snrScale=(1- (p/q)*diag(S,0).^2./diag(C,0).^2);
snrScale= max(snrScale,0); %Sometimee noise components can have SNR values below 0
%Because of variability.  We clamp these to 0.
elseif strcmpi(method,'maxsnr')
    snrScale = zeros(length(diag(S)),1);
    snrScale(end) = 1;
end


filterSNR = snrScale;

%The logic of the linear algebra is to a create a matrix A when A*data
%From right to left: Take data multiply by filter to bring it to filter
%space, weight each of the filters by the snr to scale each component, then
%finaly use the inverse of the filter basis to project back to the original
%data space. 
filterWeights = ( (filterBasis'\diag(snrScale) )*filterBasis')';
%Now apply the SVD dimension mapping to the filter:
%The linear algebra is basically as above project into the reduced
%dimension from SVD. then into the filter, then back out to the original
%data. Note this doesn't do an inverse because for the SVD orthonormal
%mapping inv(U) = U';
filterWeights = uFilter*(filterWeights * uFilter');


filteredSignal = filterWeights*signal(:,:);



filteredSignal = reshape(filteredSignal,size(signal));

%Now scale the filter to match the mean amplitudes:
meanSig = mean(signal,3);
meanSigHat = mean(filteredSignal,3);
scaleFac = meanSig(:)'/meanSigHat(:)';
 

filterWeights = scaleFac*filterWeights;
filteredSignal = scaleFac*filteredSignal;

if nargout>=3
    filteredNoise = filterWeights*noise(:,:);
    filteredNoise = reshape(filteredNoise,size(noise));
end

if nargout>=4
    filterProperties.filterWeights = filterWeights;
    filterProperties.filterSNR     = filterSNR;
    filterProperties.uFilter       = uFilter;
    filterProperties.filterBasis   = uFilter*filterBasis;
    filterProperties.nPCA   = svdNumCompsUsed;
    
end


    
    


