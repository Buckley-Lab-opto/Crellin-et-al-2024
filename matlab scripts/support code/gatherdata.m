function [mean_data, median_data, std_data, n_data] = gatherdata(I, data, xo, yo, window_side, x, y)
    extend = @(x) x(:);
    
    mean_data = [];
    median_data = [];
    std_data = [];
    n_data = [];

    for i = xo:window_side:size(I, 2)
        for j = yo:window_side:size(I, 1)
            xpos = find(x(1,:)>=j & x(1,:)<=j+window_side-1);
            ypos = find(y(:,1)>=i & y(:,1)<=i+window_side-1);
            data_box = extend(data(ypos, xpos));
            data_box(isnan(data_box)) = [];

            mean_data(end+1) = mean(data_box);
            std_data(end+1) = std(data_box);
            n_data(end+1) = numel(data_box);
            median_data(end+1) = median(data_box);
            
        end
    end
end