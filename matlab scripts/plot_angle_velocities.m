function plot_angle_velocities(I, xo, yo, window_side, x, y, vx, vy, t_id, vel_thr, thickness, pixel_s, on_time_point, off_time_point, angle_folder, filename)
    extend = @(x) x(:);
    vel_angles = atan2(vy, vx);
    vel_mags = hypot(vx, vy);
%     vel_thr = 0.001;
%     thickness = pi/16;
    
    clf
    imshow(I)
    axis_circ=[cos(0:pi/50:(2*pi))' sin(0:pi/50:(2*pi))'];
    
    for i = xo:window_side:size(I, 2)
        for j = yo:window_side:size(I, 1)
            xpos = find(x(1,:)>=j & x(1,:)<=j+window_side-1);
            ypos = find(y(:,1)>=i & y(:,1)<=i+window_side-1);
            box_angles = extend(vel_angles(ypos, xpos));
            box_mags = extend(vel_mags(ypos, xpos));
            box_data = box_angles(box_mags > vel_thr);
            [thetas, r ] = rose(box_data, numel(box_angles)/2);
            
            h=line(axis_circ(:,1).*window_side/2+j,...
                   axis_circ(:,2).*window_side/2+i,...
                   'color', 'k', 'LineWidth', 2);
    
            hold on
            if sum(box_mags > vel_thr) > numel(box_mags)/2 % only plot if there is enough velocities
                for val_i = 1:length(thetas)
                    patch([j, j+cos(thetas(val_i)-thickness).*r(val_i)./max(r)*window_side/2, j+cos(thetas(val_i)+thickness).*r(val_i)./max(r)*window_side/2],...
                          [i, i+sin(thetas(val_i)-thickness).*r(val_i)./max(r)*window_side/2, i+sin(thetas(val_i)+thickness).*r(val_i)./max(r)*window_side/2],...
                          [1 0.7 0], 'FaceAlpha', 0.5)
                end
            end
            
        end
    end


    title('Velocity angles')
    addScaleBar(pixel_s, 50)
    
    if t_id >= on_time_point && t_id <= off_time_point
        text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30, 'Color', [1 0.7 0])
    else
        text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30, 'Color', [1 0.7 0]*0.75)
    end
    
    if ~exist('filename', 'var') || isempty(filename)
        export_fig([angle_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);
    else
        export_fig([angle_folder filesep filename])
    end
end