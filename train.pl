:- ['hmmlearn.pl'].
:- ['util.pl'].

train_chunk(HmmInputFile,HmmOutputFile,1,HistorySize,FutureSize) :-
	total_inputs(SeqLen),
	chunk_size(ChunkSize),
	suspend_execution_to_file(ChunkSize, 'suspended.chr'),
	train_initial_chunk(HmmInputFile,HmmOutputFile,SeqLen,HistorySize,FutureSize),
	!.

train_chunk(_,_,ChunkCount,_,_) :-
	open('suspended.chr', read, StoreStream),
	load_store(StoreStream),
	chunks_total(TotalChunks),
	chunk_size(ChunkSize),
	SeqLen is ChunkSize * ChunkCount,
	write(sequence_length(SeqLen)),nl,
	(ChunkCount==TotalChunks ; suspend_execution_to_file(SeqLen,'suspended.chr')),
	live, % revive CHR store
	(ChunkCount==TotalChunks -> halt ; true).

% Sets up initial store and starts training with the first chunk
train_initial_chunk(HmmInputFile,HmmOutputFile,SeqLen,HistorySize,FutureSize) :-
	open(HmmInputFile, read, StoreStream),
	load_store(StoreStream),
	outputfile(HmmOutputFile),
	history_size(HistorySize),
	future_size(FutureSize),
	sequence_length(SeqLen),
	OnePercent is SeqLen // 100,
	progress_indication(OnePercent),
	start_training.