function mtx  = coord2matrix(x, y, greyno)

% fills in area betwen the points
hold on
rectangle(Position=[0,0,562,762], FaceColor=[1 1 1], EdgeColor=[1 1 1])
p = fill(x,y,[0,0,1]);
p.EdgeColor = [0,0,1];
xlim([0 562])
ylim([0 762])
set(gcf, 'Position', [100, 100, 562, 762], ...
    'Color', [0 0 0]);
axis off
box off
hold off

% creates a uint8 of the last opened plot and close the plot
frame = getframe(gcf);
close all
% focus on one colour layer
data  = frame.cdata(:,:,2);
% delete all rows and columns that ONLY contain 0
data( all(~data,2), : ) = [];
data( :, all(~data,1) ) = [];
% make sure the image has the correct size
data = imresize(data, [762 562]);

% create the matrix and reset the colour scheme
mtx = double(data);
mtx(mtx < 245)  = greyno;
mtx(mtx >= 245) = 0;

% set the outlines to zero as well
mtx(1:end,1)    = 0;
mtx(1,1:end)    = 0;
mtx(end,1:end)  = 0;
mtx(1:end,end)  = 0;

end
