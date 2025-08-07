% Generate split transition matrices: susceptible (S) and LTBI (L) with shared death/emigration

% Define state names
S_states = {'p_sus','p_sus_notest','p_sus_nf','p_sus_nbt','p_sus_nct','p_sus_tc',...
    'p_sus_sae','p_sus_sae_death','p_sus_no_risk'};

L_states = {'p_ltbi','p_ltbi_notest','p_ltbi_nf','p_ltbi_nbt','p_ltbi_nct','p_ltbi_tc',...
    'p_ltbi_sae','p_ltbi_sae_death','p_ltbi_ongoing_risk','p_ltbi_no_risk',...
    'p_tb','p_tbr','p_tb_death'};

terminal_states = {'p_death', 'p_emigrate'};

% All state names
state_names_sus = [S_states, terminal_states];
state_names_ltbi = [L_states, terminal_states];

idx_S = @(s) find(strcmp(state_names_sus, s));
idx_L = @(s) find(strcmp(state_names_ltbi, s));

n_S = length(state_names_sus);
n_L = length(state_names_ltbi);

T_S = cell(n_S);
T_L = cell(n_L);

param = p.cascade_of_care;
param.ATTEND = param.att;
param.ATTENDSCREEN = param.attscreen;
param.BEGINTREAT = param.begintrt;
param.TESTSN = param.snqftgit;
param.TESTSP = param.spqftgit;
%% Susceptible model (T_S)
T_S{idx_S('p_sus_notest'), idx_S('p_sus')} = '1 - param.ATTENDSCREEN';
T_S{idx_S('p_sus_nf'), idx_S('p_sus')} = 'param.ATTENDSCREEN * (1 - (1 - param.TESTSP) * param.ATTEND)';
T_S{idx_S('p_sus_nbt'), idx_S('p_sus')} = 'param.ATTENDSCREEN * (1 - param.TESTSP) * param.ATTEND * (1 - param.BEGINTREAT)';
T_S{idx_S('p_sus_nct'), idx_S('p_sus')} = 'param.ATTENDSCREEN * (1 - param.TESTSP) * param.ATTEND * param.BEGINTREAT * (1 - param.TREATCOMPLETE - param.SAE)';
T_S{idx_S('p_sus_tc'), idx_S('p_sus')} = 'param.ATTENDSCREEN * (1 - param.TESTSP) * param.ATTEND * param.BEGINTREAT * param.TREATCOMPLETE';
T_S{idx_S('p_sus_sae'), idx_S('p_sus')} = 'param.ATTENDSCREEN * (1 - param.TESTSP) * param.ATTEND * param.BEGINTREAT * param.SAE';
T_S{idx_S('p_sus_sae_death'), idx_S('p_sus_sae')} = 'param.SAEMR';
T_S{idx_S('p_sus_sae_death'), idx_S('p_sus_sae_death')} = '1';

sus_end_states = {'p_sus_notest','p_sus_nf','p_sus_nbt','p_sus_nct','p_sus_tc','p_sus_sae','p_sus_no_risk'};
for i = 1:length(sus_end_states)
    T_S{idx_S('p_sus_no_risk'), idx_S(sus_end_states{i})} = 'CMP';
end

%% LTBI model (T_L)
T_L{idx_L('p_ltbi_notest'), idx_L('p_ltbi')} = '(1 - param.ATTENDSCREEN) * (1 - (param.RR * param.RRADJUST))';
T_L{idx_L('p_ltbi_nf'), idx_L('p_ltbi')} = 'param.ATTENDSCREEN * (1 - (param.TESTSN * param.ATTEND) - (param.RR * param.RRADJUST * (1 - ((param.TESTSN * param.ATTEND * param.BEGINTREAT * param.TREATR) * (1 - param.TIMETOTREAT)))) )';
T_L{idx_L('p_ltbi_nbt'), idx_L('p_ltbi')} = 'param.ATTENDSCREEN * param.TESTSN * param.ATTEND * (1 - param.BEGINTREAT)';
T_L{idx_L('p_ltbi_nct'), idx_L('p_ltbi')} = 'param.ATTENDSCREEN * param.TESTSN * param.ATTEND * param.BEGINTREAT * (1 - param.TREATCOMPLETE - param.SAE)';
T_L{idx_L('p_ltbi_tc'), idx_L('p_ltbi')} = 'param.ATTENDSCREEN * param.TESTSN * param.ATTEND * param.BEGINTREAT * param.TREATCOMPLETE';
T_L{idx_L('p_ltbi_sae'), idx_L('p_ltbi')} = 'param.ATTENDSCREEN * param.TESTSN * param.ATTEND * param.BEGINTREAT * param.SAE';
T_L{idx_L('p_ltbi_sae_death'), idx_L('p_ltbi_sae')} = 'param.SAEMR';
T_L{idx_L('p_ltbi_sae_death'), idx_L('p_ltbi_sae_death')} = '1';

ltbi_paths = {'p_ltbi_notest','p_ltbi_nf','p_ltbi_nbt','p_ltbi_nct','p_ltbi_tc','p_ltbi_sae','p_ltbi_ongoing_risk'};
for i = 1:length(ltbi_paths)
    T_L{idx_L('p_ltbi_ongoing_risk'), idx_L(ltbi_paths{i})} = 'CMP';
end

T_L{idx_L('p_ltbi_no_risk'), idx_L('p_ltbi_nct')} = 'param.PART_TREAT_EFFICACY';
T_L{idx_L('p_ltbi_no_risk'), idx_L('p_ltbi_tc')} = 'param.FULL_TREAT_EFFICACY - (param.MR * param.FULL_TREAT_EFFICACY)';
T_L{idx_L('p_ltbi_no_risk'), idx_L('p_ltbi_no_risk')} = 'CMP';

% Reactivation
T_L{idx_L('p_tb'), idx_L('p_ltbi')} = '((1 - param.ATTENDSCREEN) * param.RR * param.RRADJUST) + (param.ATTENDSCREEN * param.RR * param.RRADJUST * (1 - ((param.TESTSN * param.ATTEND * param.BEGINTREAT * param.TREATR) * (1 - param.TIMETOTREAT))))';
T_L{idx_L('p_tb'), idx_L('p_ltbi_notest')} = 'param.RR * param.RRADJUST';
T_L{idx_L('p_tb'), idx_L('p_ltbi_nf')} = 'param.RR * param.RRADJUST';
T_L{idx_L('p_tb'), idx_L('p_ltbi_nbt')} = 'param.RR * param.RRADJUST';
T_L{idx_L('p_tb'), idx_L('p_ltbi_nct')} = '(1 - param.PART_TREAT_EFFICACY) * param.RR * param.RRADJUST';
T_L{idx_L('p_tb'), idx_L('p_ltbi_tc')} = '(1 - param.FULL_TREAT_EFFICACY - param.MR * (1 - param.FULL_TREAT_EFFICACY)) * param.RR * param.RRADJUST';
T_L{idx_L('p_tb'), idx_L('p_ltbi_sae')} = 'param.RR * param.RRADJUST';
T_L{idx_L('p_tb'), idx_L('p_ltbi_ongoing_risk')} = 'param.RR * param.RRADJUST';

% TB onward
T_L{idx_L('p_tbr'), idx_L('p_tb')} = 'CMP';
T_L{idx_L('p_tbr'), idx_L('p_tbr')} = 'CMP';
T_L{idx_L('p_tb_death'), idx_L('p_tb')} = 'param.TBMR';
T_L{idx_L('p_tb_death'), idx_L('p_tb_death')} = '1';

% Shared mortality and emigration
for i = 1:n_S
    if ~strcmp(state_names_sus{i}, 'p_death') && ~strcmp(state_names_sus{i}, 'p_emigrate')
        T_S{idx_S('p_death'), i} = 'param.MR';
        T_S{idx_S('p_emigrate'), i} = 'param.EMIGRATE';
    end
end
T_S{idx_S('p_death'), idx_S('p_death')} = '1';
T_S{idx_S('p_emigrate'), idx_S('p_emigrate')} = '1';

for i = 1:n_L
    if ~strcmp(state_names_ltbi{i}, 'p_death') && ~strcmp(state_names_ltbi{i}, 'p_emigrate')
        T_L{idx_L('p_death'), i} = 'param.MR';
        T_L{idx_L('p_emigrate'), i} = 'param.EMIGRATE';
    end
end
T_L{idx_L('p_death'), idx_L('p_death')} = '1';
T_L{idx_L('p_emigrate'), idx_L('p_emigrate')} = '1';

% Fill in empty cells with '0'
for i = 1:n_L
    for j = 1:n_L
        if isempty(T_L{i, j})
            T_L{i, j} = '0';
        end
    end
end

% Create header row (top and left)
header = ['FROM\TO', state_names_ltbi];
csv_out = [header; [state_names_ltbi', T_L]];

% Save to CSV
writecell(csv_out, 'transition_matrix_expressions_LTBI.csv');

% Fill in empty cells with '0'
for i = 1:n_S
    for j = 1:n_S
        if isempty(T_S{i, j})
            T_S{i, j} = '0';
        end
    end
end

% Create header row (top and left)
header = ['FROM\TO', state_names_sus];
csv_out = [header; [state_names_sus', T_S]];

% Save to CSV
writecell(csv_out, 'transition_matrix_expressions_S.csv');



