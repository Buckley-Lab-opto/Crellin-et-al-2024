function addScaleBar(pixel_s, realDist)

%     pixel_s = 0.65;
%     pixel_s = 0.5682;
%     pixel_s = 0.2403;
%     realDist = 500; %uM
%     realDist = 250;
    zlims = zlim;
    scaleBarLength = realDist/pixel_s;%in original image pixels
    rotation = get(gca, 'view');
    
    if isequal(rotation, [0,90])
        ydir = 'Ydir';
        xdir = 'Xdir';
        xlims = xlim;
        ylims = ylim;
    
        if strcmp(get(gca, ydir),'reverse')
           scaleBarOriginY = [ylims(2)-20];
        else
            scaleBarOriginY = [ylims(1)+20];
        end

        hold on
        if strcmp(get(gca, xdir),'reverse')

            scaleBarOriginX = [xlims(1)+scaleBarLength*1.25];%
            hSB = plot3([scaleBarOriginX; scaleBarOriginX-scaleBarLength],[scaleBarOriginY; scaleBarOriginY], [zlims(2); zlims(2)]);

        else
            scaleBarOriginX = [xlims(2)-scaleBarLength*1.25];%
            hSB = plot3([scaleBarOriginX; scaleBarOriginX+scaleBarLength],[scaleBarOriginY; scaleBarOriginY], [zlims(2); zlims(2)]);        
        end
    
    else
        ydir = 'Xdir';
        xdir = 'ydir';
        xlims = ylim;
        ylims = xlim;
        
        if strcmp(get(gca, ydir),'reverse')
           scaleBarOriginY = [ylims(1)+20];
        else
            scaleBarOriginY = [ylims(2)-20];
        end

        hold on
        if strcmp(get(gca, xdir),'reverse')

            scaleBarOriginX = [xlims(1)+scaleBarLength*2];%
            hSB = plot3([scaleBarOriginY; scaleBarOriginY], [scaleBarOriginX; scaleBarOriginX-scaleBarLength], [zlims(2); zlims(2)]);

        else
            scaleBarOriginX = [xlims(2)-scaleBarLength*2];%
            hSB = plot3([scaleBarOriginY; scaleBarOriginY],[scaleBarOriginX; scaleBarOriginX+scaleBarLength], [zlims(2); zlims(2)]);        
        end
    end
         
    hSB.LineStyle = '-';
    hSB.LineWidth = 3;
    hSB.Color = [0 0 0];
%     hSB.Color = [1 1 1];

end