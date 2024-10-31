function [vel_mean, vel_std, vang_mean, vang_std, iso_mean, iso_std, iso_x_mean, iso_x_std, iso_y_mean, iso_y_std, ani_mean, ani_std, aniang_mean, aniang_std, str_mean, str_std, apstr_mean, apstr_std, mlstr_mean, mlstr_std, T] = grabNSWECdata(image_path, piv_path, path2roi, path2datagrid, param, idx, vel_mean, vel_std, vang_mean, vang_std, iso_mean, iso_std, iso_x_mean, iso_x_std, iso_y_mean, iso_y_std, ani_mean, ani_std, aniang_mean, aniang_std, str_mean, str_std, apstr_mean, apstr_std, mlstr_mean, mlstr_std)
    
    %image_path, piv_path, path2roi

    load(path2datagrid);

    S = openTiffStack(image_path);
    S = uint8(rescale(S, 0, 255));
    
    vf = load(piv_path);    
    x = vf.x{1};
    y = vf.y{1};
    spatial_filt_size = param.spatial_filt_size;
    crop = floor(spatial_filt_size/2)+1;
    
    roi = imread(path2roi);
    roi = roi(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    roi_perim = bwperim(roi);
    roi_perim = imdilate(roi_perim, strel('disk', 4));
    roi_props = regionprops(roi>0, 'centroid', 'area');
    roi_centre = roi_props.Centroid;
    window_side = sqrt(roi_props.Area);
    
    % get positions so the roi ends up in the center of a window of
    % interest
    nx = (roi_centre(2)-window_side)/window_side;
    ny = (roi_centre(1)-window_side)/window_side;
    xo = round(mod(abs(nx),1)*window_side);
    yo = round(mod(abs(ny),1)*window_side);
        
    I = S(:,:,1);
    windows_x = [xo:window_side:size(I, 2)-window_side];
    windows_y = [yo:window_side:size(I, 1)-window_side];
    Nc = [round(roi_centre(1)), round(roi_centre(2))-window_side];
    Sc = [round(roi_centre(1)), round(roi_centre(2))+window_side];
    Wc = [round(roi_centre(1))-window_side, round(roi_centre(2))];
    Ec = [round(roi_centre(1))+window_side, round(roi_centre(2))];
    
    coords = [];
    coords_idx = [];
    counter = 1;
    for i = windows_x
        for j = windows_y
            coords(end+1, 1:2) = [j, i];
            coords_idx(end+1) = counter;
            counter = counter+1;
        end
    end
    
    [~, Ni] = min(pdist2(Nc, coords));
    [~, Si] = min(pdist2(Sc, coords));
    [~, Wi] = min(pdist2(Wc, coords));
    [~, Ei] = min(pdist2(Ec, coords));
    [~, Ci] = min(pdist2(roi_centre, coords));
    
    for t_id = 2:length(data_grid)
        vel_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.vel(1,Ni); %[mean_data, median_data, std_data, n_data]
        vel_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.vel(3,Ni); %[mean_data, median_data, std_data, n_data]

        vel_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.vel(1,Si); %[mean_data, median_data, std_data, n_data]
        vel_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.vel(3,Si); %[mean_data, median_data, std_data, n_data]

        vel_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.vel(1,Wi); %[mean_data, median_data, std_data, n_data]
        vel_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.vel(3,Wi); %[mean_data, median_data, std_data, n_data]

        vel_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.vel(1,Ei); %[mean_data, median_data, std_data, n_data]
        vel_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.vel(3,Ei); %[mean_data, median_data, std_data, n_data]

        vel_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.vel(1,Ci); %[mean_data, median_data, std_data, n_data]
        vel_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.vel(3,Ci); %[mean_data, median_data, std_data, n_data]

        vang_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.vel_angle(1,Ni); %[mean_data, median_data, std_data, n_data]
        vang_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.vel_angle(3,Ni); %[mean_data, median_data, std_data, n_data]

        vang_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.vel_angle(1,Si); %[mean_data, median_data, std_data, n_data]
        vang_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.vel_angle(3,Si); %[mean_data, median_data, std_data, n_data]

        vang_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.vel_angle(1,Wi); %[mean_data, median_data, std_data, n_data]
        vang_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.vel_angle(3,Wi); %[mean_data, median_data, std_data, n_data]

        vang_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.vel_angle(1,Ei); %[mean_data, median_data, std_data, n_data]
        vang_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.vel_angle(3,Ei); %[mean_data, median_data, std_data, n_data]

        vang_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.vel_angle(1,Ci); %[mean_data, median_data, std_data, n_data]
        vang_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.vel_angle(3,Ci); %[mean_data, median_data, std_data, n_data]
        
        iso_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.iso(1,Ni); %[mean_data, median_data, std_data, n_data]
        iso_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.iso(3,Ni); %[mean_data, median_data, std_data, n_data]

        iso_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.iso(1,Si); %[mean_data, median_data, std_data, n_data]
        iso_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.iso(3,Si); %[mean_data, median_data, std_data, n_data]

        iso_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.iso(1,Wi); %[mean_data, median_data, std_data, n_data]
        iso_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.iso(3,Wi); %[mean_data, median_data, std_data, n_data]

        iso_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.iso(1,Ei); %[mean_data, median_data, std_data, n_data]
        iso_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.iso(3,Ei); %[mean_data, median_data, std_data, n_data]

        iso_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.iso(1,Ci); %[mean_data, median_data, std_data, n_data]
        iso_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.iso(3,Ci); %[mean_data, median_data, std_data, n_data]

        iso_x_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.iso_x(1,Ni); %[mean_data, median_data, std_data, n_data]
        iso_x_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.iso_x(3,Ni); %[mean_data, median_data, std_data, n_data]

        iso_x_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.iso_x(1,Si); %[mean_data, median_data, std_data, n_data]
        iso_x_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.iso_x(3,Si); %[mean_data, median_data, std_data, n_data]

        iso_x_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.iso_x(1,Wi); %[mean_data, median_data, std_data, n_data]
        iso_x_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.iso_x(3,Wi); %[mean_data, median_data, std_data, n_data]

        iso_x_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.iso_x(1,Ei); %[mean_data, median_data, std_data, n_data]
        iso_x_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.iso_x(3,Ei); %[mean_data, median_data, std_data, n_data]

        iso_x_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.iso_x(1,Ci); %[mean_data, median_data, std_data, n_data]
        iso_x_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.iso_x(3,Ci); %[mean_data, median_data, std_data, n_data]

        iso_y_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.iso_y(1,Ni); %[mean_data, median_data, std_data, n_data]
        iso_y_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.iso_y(3,Ni); %[mean_data, median_data, std_data, n_data]

        iso_y_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.iso_y(1,Si); %[mean_data, median_data, std_data, n_data]
        iso_y_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.iso_y(3,Si); %[mean_data, median_data, std_data, n_data]

        iso_y_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.iso_y(1,Wi); %[mean_data, median_data, std_data, n_data]
        iso_y_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.iso_y(3,Wi); %[mean_data, median_data, std_data, n_data]

        iso_y_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.iso_y(1,Ei); %[mean_data, median_data, std_data, n_data]
        iso_y_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.iso_y(3,Ei); %[mean_data, median_data, std_data, n_data]

        iso_y_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.iso_y(1,Ci); %[mean_data, median_data, std_data, n_data]
        iso_y_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.iso_y(3,Ci); %[mean_data, median_data, std_data, n_data]

        ani_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.ani(1,Ni); %[mean_data, median_data, std_data, n_data]
        ani_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.ani(3,Ni); %[mean_data, median_data, std_data, n_data]

        ani_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.ani(1,Si); %[mean_data, median_data, std_data, n_data]
        ani_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.ani(3,Si); %[mean_data, median_data, std_data, n_data]

        ani_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.ani(1,Wi); %[mean_data, median_data, std_data, n_data]
        ani_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.ani(3,Wi); %[mean_data, median_data, std_data, n_data]

        ani_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.ani(1,Ei); %[mean_data, median_data, std_data, n_data]
        ani_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.ani(3,Ei); %[mean_data, median_data, std_data, n_data]

        ani_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.ani(1,Ci); %[mean_data, median_data, std_data, n_data]
        ani_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.ani(3,Ci); %[mean_data, median_data, std_data, n_data]

        aniang_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.ani_angle(1,Ni); %[mean_data, median_data, std_data, n_data]
        aniang_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.ani_angle(3,Ni); %[mean_data, median_data, std_data, n_data]

        aniang_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.ani_angle(1,Si); %[mean_data, median_data, std_data, n_data]
        aniang_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.ani_angle(3,Si); %[mean_data, median_data, std_data, n_data]

        aniang_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.ani_angle(1,Wi); %[mean_data, median_data, std_data, n_data]
        aniang_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.ani_angle(3,Wi); %[mean_data, median_data, std_data, n_data]

        aniang_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.ani_angle(1,Ei); %[mean_data, median_data, std_data, n_data]
        aniang_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.ani_angle(3,Ei); %[mean_data, median_data, std_data, n_data]

        aniang_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.ani_angle(1,Ci); %[mean_data, median_data, std_data, n_data]
        aniang_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.ani_angle(3,Ci); %[mean_data, median_data, std_data, n_data]

        str_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.str(1,Ni); %[mean_data, median_data, std_data, n_data]
        str_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.str(3,Ni); %[mean_data, median_data, std_data, n_data]

        str_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.str(1,Si); %[mean_data, median_data, std_data, n_data]
        str_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.str(3,Si); %[mean_data, median_data, std_data, n_data]

        str_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.str(1,Wi); %[mean_data, median_data, std_data, n_data]
        str_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.str(3,Wi); %[mean_data, median_data, std_data, n_data]

        str_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.str(1,Ei); %[mean_data, median_data, std_data, n_data]
        str_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.str(3,Ei); %[mean_data, median_data, std_data, n_data]

        str_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.str(1,Ci); %[mean_data, median_data, std_data, n_data]
        str_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.str(3,Ci); %[mean_data, median_data, std_data, n_data]

        apstr_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.apstr(1,Ni); %[mean_data, median_data, std_data, n_data]
        apstr_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.apstr(3,Ni); %[mean_data, median_data, std_data, n_data]

        apstr_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.apstr(1,Si); %[mean_data, median_data, std_data, n_data]
        apstr_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.apstr(3,Si); %[mean_data, median_data, std_data, n_data]

        apstr_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.apstr(1,Wi); %[mean_data, median_data, std_data, n_data]
        apstr_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.apstr(3,Wi); %[mean_data, median_data, std_data, n_data]

        apstr_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.apstr(1,Ei); %[mean_data, median_data, std_data, n_data]
        apstr_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.apstr(3,Ei); %[mean_data, median_data, std_data, n_data]

        apstr_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.apstr(1,Ci); %[mean_data, median_data, std_data, n_data]
        apstr_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.apstr(3,Ci); %[mean_data, median_data, std_data, n_data]

        mlstr_mean(idx,200-param.on_time_point+t_id,1) = data_grid{t_id}.mlstr(1,Ni); %[mean_data, median_data, std_data, n_data]
        mlstr_std(idx, 200-param.on_time_point+t_id,1) = data_grid{t_id}.mlstr(3,Ni); %[mean_data, median_data, std_data, n_data]

        mlstr_mean(idx,200-param.on_time_point+t_id,2) = data_grid{t_id}.mlstr(1,Si); %[mean_data, median_data, std_data, n_data]
        mlstr_std(idx, 200-param.on_time_point+t_id,2) = data_grid{t_id}.mlstr(3,Si); %[mean_data, median_data, std_data, n_data]

        mlstr_mean(idx,200-param.on_time_point+t_id,3) = data_grid{t_id}.mlstr(1,Wi); %[mean_data, median_data, std_data, n_data]
        mlstr_std(idx, 200-param.on_time_point+t_id,3) = data_grid{t_id}.mlstr(3,Wi); %[mean_data, median_data, std_data, n_data]

        mlstr_mean(idx,200-param.on_time_point+t_id,4) = data_grid{t_id}.mlstr(1,Ei); %[mean_data, median_data, std_data, n_data]
        mlstr_std(idx, 200-param.on_time_point+t_id,4) = data_grid{t_id}.mlstr(3,Ei); %[mean_data, median_data, std_data, n_data]

        mlstr_mean(idx,200-param.on_time_point+t_id,5) = data_grid{t_id}.mlstr(1,Ci); %[mean_data, median_data, std_data, n_data]
        mlstr_std(idx, 200-param.on_time_point+t_id,5) = data_grid{t_id}.mlstr(3,Ci); %[mean_data, median_data, std_data, n_data]

    end

    unnest = @(x) [x{:}];
    l = cellfun(@(z) cellfun(@(y) cellfun(@(x) [x '_' y '_' z], {'N', 'S', 'W', 'E', 'C'}, 'UniformOutput', false), {'vel', 'vang', 'iso', 'ani', 'iso_x', 'iso_y', 'str', 'ap_str', 'ml_str'}, 'UniformOutput', false), {'mean', 'std'}, 'UniformOutput', false);
    l = unnest(unnest(l));
    types = cell(1, length(l));
    types(:) = {'double'};
    timepoints = [-199:800];
    T = table('Size', [size(vel_mean(idx,:,:),2), length(l)],...
              'VariableNames', l,...
              'VariableType', types,...
              'RowNames', sprintfc('%d', timepoints)');

    loc_i = 1;
    for loc = {'N', 'S', 'W', 'E', 'C'}
        for mes = {'vel', 'vang', 'iso', 'iso_x', 'iso_y', 'ani', 'aniang', 'str', 'apstr', 'mlstr'}
            for stat = {'mean', 'std'}
                v = eval([mes{1} '_' stat{1}]);
                T.([loc{1} '_' mes{1} '_' stat{1}]) = squeeze(v(idx, :, loc_i))';
            end
            
        end
        loc_i = loc_i + 1;
    end
    


end