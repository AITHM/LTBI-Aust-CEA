function T_eval = evaluate_transition_matrix(T_expr, param, state_names)
    n = length(state_names);
    T_eval = zeros(n);

    % Loop through all cells
    for i = 1:n
        row_sum = 0;
        cmp_index = [];

        for j = 1:n
            expr = T_expr{i, j};

            if isequal(expr, 'CMP')
                cmp_index(end + 1) = j;
                continue;
            end

            try
                if isnumeric(expr)
                    val = expr;
                else
                    val = eval(expr);  % Use 'param' from caller's scope
                end
            catch
                warning("Could not evaluate expression at (%d, %d): %s", i, j, expr);
                val = 0;
            end

            T_eval(i, j) = val;
            row_sum = row_sum + val;
        end

        % Distribute complement if CMP present
        if ~isempty(cmp_index)
            cmp_val = (1 - row_sum);
            if cmp_val < 0
                warning("Negative complement in row %d: %.3f", i, cmp_val);
                cmp_val = 0;
            end
            for j = cmp_index
                T_eval(i, j) = cmp_val / length(cmp_index);
            end
        end
    end
end
