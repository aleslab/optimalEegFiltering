function out = transformForTimeFilter(input)

nCh = size(input,1);
nT  = size(input,2);
nTr = size(input,3);

nTaps = 40;
%timeWindow = tukeywin(400,.2); %A tukey window is flat in the time-domain

out = zeros(nCh,nTaps,nT-nTaps,nTr);

for iCh = 1:nCh,
    for iTr = 1:nTr
    
    thisWave = input(iCh,:,iTr);
    tmp=(toeplitz([input(iCh,1,iTr); zeros(nTaps-1,1)],thisWave));    
    out(iCh,:,:,iTr) = tmp(:,nTaps+1:end);
    end
 
end

tw=tukeywin(size(out,3),.25);

out = shiftdim(tw',-1).*out;

out = padarray(out,[ 0 0 2*nTaps 0]);

out = permute(out,[3 4 1 2]);
out = permute(out(:,:,:),[3 1 2]);
