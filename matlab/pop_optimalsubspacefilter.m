% pop_optimalsubspacefilter() - Filter data using learned optimal subspaces 
%
% Usage:
%   >> [EEG, com, b] = pop_optimalsubspacefilter(EEG,[method,noiseWin,sigWin,dimensionToFilter]); % pop-up window mode
%
% Inputs:
%   EEG       - EEGLAB EEG structure
%
% Optional inputs:
%   method - {['mse', 'maxsnr'} Optimize squared error ('mse') or SNR
%            ('maxsnr'). Default: 'mse'
%   dimensionToFilter - {'channel','time'} What dimension to use for
%                       filtering. Default: 'channel'
%
% Outputs:
%   EEG       - filtered EEGLAB EEG structure
%   com       - history string
%   b         - filter coefficients
%
% Note:

%
% Author: Justin Ales, University of St Andrews, 2019
%

%123456789012345678901234567890123456789012345678901234567890123456789012

% Copyright (C) 2019 Justin Ales, 
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

function [EEG, com] = pop_optimalsubspacefilter(EEG,method,noiseWin,sigWin,dimensionToFilter)

com = '';

if nargin < 1
    help pop_optimalsubspacefilter;
    return
end
if isempty(EEG.data)
    error('Cannot filter empty dataset.');
end



% GUI
if nargin < 2
    
    noiseWin = [num2str(EEG(1).xmin*1000,4) ' 0'];
    sigWin = ['0 ' num2str(EEG(1).xmax*1000,4)];
    if EEG(1).xmin*1000 >= 0
        noiseWin = '[ ]';
    end
    
    geometry = {[2 1] [2 1] [2 2] [2 2]};
    geomvert = [1 1 1 1];

    uilist = {{ 'style' 'text' 'string' 'Noise Window latency range ([min max] in ms):' } ...
               {'style', 'edit', 'string', noiseWin,'tag','noiseWin'} ...
              { 'style' 'text' 'string' 'Signal Window latency range ([min max] in ms):' } ...
              {'style', 'edit', 'string', sigWin,'tag','sigWin'} ...
              { 'style' 'text' 'string' 'Choose What to Optimize: ' } ...
              { 'style', 'popupmenu', 'string', 'Minimize Squared Error|Maximize SNR','value',1,'tag','method' } ...
              { 'style' 'text' 'string' 'What dimension to filter: ' } ...
              { 'style', 'popupmenu', 'string', 'Spatial (channels)','value',1,'tag','dimensionToFilter' } ...
              };

    [result userdata err structOut] = inputgui('geometry', geometry, 'geomvert', geomvert, 'uilist', uilist, 'title', 'Optimal Subspace Filter the data -- pop_optimalsubspacefilter()', 'helpcom', 'pophelp(''pop_optimalsubspacefilter'')');

    if isempty(result), return; end   
    methodList={'mse','maxsnr'};
    method = methodList{structOut.method};
    dimList = {'channel','time'};
    dimensionToFilter = dimList{structOut.dimensionToFilter};
    noiseWin = str2num(structOut.noiseWin);
    sigWin   = str2num(structOut.sigWin);
    
else
    
    if nargin < 3 || isempty(method)
        method = 'mse';
    end
    if nargin < 4 || isempty(noiseWin)
        noiseWin = [num2str(EEG(1).xmin*1000,4) ' 0'];

    end
    if nargin < 5 || isempty(sigWin)
        sigWin = ['0 ' num2str(EEG(1).xmax*1000,4)];
    end
    if nargin < 6 || isempty(dimensionToFilter)
        dimensionToFilter = 'channel';
    end
end

% Constants

% Check arguments

%Translate
if (sigWin(1) < EEG.xmin*1000) || (sigWin(2) > EEG.xmax*1000)
        error('pop_optimalsubspacefilter(): Bad time range');
end;

[~, sigWinIdx]=min(((EEG.times-sigWin').^2),[],2);
sigPointRange = sigWinIdx(1):sigWinIdx(2);

[~, noiseWinIdx]=min(((EEG.times-noiseWin').^2),[],2);
noisePointRange = noiseWinIdx(1):noiseWinIdx(2);


    
   

%Put some checks in here-
sigDat = EEG.data(:,sigPointRange,:);
noiseDat = EEG.data(:,noisePointRange,:);

if strcmpi('channel',dimensionToFilter) %This section does filtering by channel components
[filterWeights, ~, ~, filtProp] = gsvdFilter(sigDat,noiseDat,method);

EEG.data(:,:) = filterWeights*EEG.data(:,:);

elseif strcmpi('time',dimensionToFilter) %This section does filtering by channel components
error('pop_optimalsubspacefilter: Temporal filtering not yet implemented')
    
end

% fprintf('pop_optimalsubspacefilter() - performing %d point %s filtering.\n', filtorder + 1, filterTypeArray{revfilt + 1, length(edgeArray)})
% fprintf('pop_optimalsubspacefilter() - transition band width: %.4g Hz\n', df)
% fprintf('pop_optimalsubspacefilter() - passband edge(s): %s Hz\n', mat2str(edgeArray))



% History string
com = sprintf('%s = pop_optimalsubspacefilter(%s, %s);', inputname(1), inputname(1), vararg2str({method,noiseWin,sigWin,dimensionToFilter}));

end
