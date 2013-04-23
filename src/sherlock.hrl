%% sherlock.hrl

%% Copyright (c) 2009 Whoomph Software AB (joearms@gmail.com). All rights reserved.
%% For license read the top level LICENSE file

-record(post,
	{id,messageId,from,date,subject,keywords=[],body,scores=0}).
