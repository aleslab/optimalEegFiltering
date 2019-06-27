% eegplugin_optimalsubspace() - EEGLAB plugin for filtering data using
%                               optimal subspace filters
%
% Usage:
%   >> eegplugin_optimalsubspace(fig, trystrs, catchstrs);
%
% Inputs:
%   fig        - [integer]  EEGLAB figure
%   trystrs    - [struct] "try" strings for menu callbacks.
%   catchstrs  - [struct] "catch" strings for menu callbacks.
%
% Author: Justin Ales, University of St Andrews, 2019

%123456789012345678901234567890123456789012345678901234567890123456789012

% Copyright (C) 2019 Justin Ales
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

function vers = eegplugin_optimalsubspace(fig, trystrs, catchstrs)

    vers = 'optimalsubspace0.1';
    if nargin < 3
        error('eegplugin_optimalsubspace requires 3 arguments');
    end

    % add folder to path
    % -----------------------
    if ~exist('pop_optimalsubspacefilter')
        p = which('eegplugin_optimalsubspace');
        p = p(1:findstr(p,'eegplugin_optimalsubspace.m')-1);
        addpath([p vers]);
    end

    % find import data menu
    % ---------------------
    menu = findobj(fig, 'tag', 'filter');

    % menu callbacks
    % --------------
    comoptimalsubspacefilter = [trystrs.no_check '[EEG LASTCOM] = pop_optimalsubspacefilter(EEG);' catchstrs.new_and_hist];

    % create menus if necessary
    % -------------------------
    uimenu( menu, 'Label', 'Optimal Subspace Filter', ...
        'CallBack', comoptimalsubspacefilter,...
        'Separator', 'on', 'position', 1,'userdata', 'continuous:off');

  