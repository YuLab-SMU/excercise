library(reticulate)
# 确保你的Python环境中有colabfold
# use_python("/usr/bin/python3")

# 在R中运行Python代码
py_run_string("
from colabfold.batch import get_queries, run
queries = get_queries(['AUF80930.fasta'])
run(queries, result_dir='./results', use_templates=False, msa_mode='MMseqs2 (UniRef+Environmental)', num_models=1, relax_mode='none')
")

# 回到R，读取预测结果
predicted_pdb <- read.pdb("./results/example_sequence_unrelaxed_rank_1.pdb")
view(predicted_pdb)