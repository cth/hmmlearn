% Misc utility rules

chunk_file_basename(datachunk).

chunk_file_name(ChunkNo,FileName) :-
	chunk_file_basename(ChunkBaseName),
	atom_concat(ChunkBaseName, ChunkNo, WithNo),
	atom_concat(WithNo, '.pl',FileName).
	
	
write_chunk_meta_data(TotalRulesRead, RulesInLastChunk, TotalChunks) :-
	chunk_file_name('_metadata',MetaFile),
	open(MetaFile,write,MetaStream),
	current_output(CurrentOutput),
	set_output(MetaStream),
	write(total_inputs(TotalRulesRead)), write('.'),nl,
	write(last_chunk_size(RulesInLastChunk)), write('.'), nl,
	write(chunks_total(TotalChunks)), write('.'),nl,
	chunk_size(ChunkSize),
	write(chunk_size(ChunkSize)), write('.'),nl,
	set_output(CurrentOutput),
	close(MetaStream).
	
read_rules(InputStream, OutputStream,RulesInChunk,ChunkNo,TotalRulesRead) :-
	chunk_size(ChunkSize),
	RulesInChunk < ChunkSize,
    read(InputStream, T),
	!,
    ((T == end_of_file) ->
		close(InputStream),
		close(OutputStream),
		((RulesInChunk==0) ->
			% Delete last chunkfile if we wrote nothing to it
			chunk_file_name(ChunkNo,ChunkFile),
			delete_file(ChunkFile),
			ChunksTotal is ChunkNo - 1,
			write_chunk_meta_data(TotalRulesRead,ChunkSize,ChunksTotal)
			;
			write_chunk_meta_data(TotalRulesRead,RulesInChunk,ChunkNo)
		)
    ;
		NextTotalRulesRead is TotalRulesRead + 1,
		NextRulesInChunk is RulesInChunk + 1,
		current_output(CurrentOutput),
		set_output(OutputStream),
		portray_clause(T),
		set_output(CurrentOutput),	
        read_rules(InputStream, OutputStream, NextRulesInChunk, ChunkNo, NextTotalRulesRead)
    ),
	!.

read_rules(InputStream,OutputStream,NoRulesRead,ChunkNo,TotalRulesRead) :-
	chunk_size(NoRulesRead),
	close(OutputStream),
	NextChunkNo is ChunkNo + 1,
	nl,write('Wrote chunk: '), write(ChunkNo),nl,
	chunk_file_name(NextChunkNo,FileName),
	open(FileName,write,NewOutputStream),
	!,
	read_rules(InputStream,NewOutputStream,0,NextChunkNo,TotalRulesRead).

chunksplit_data_file(InputFile) :-
	open(InputFile, read, InputStream),
	chunk_file_name(1,OutputFile),
	open(OutputFile,write,OutputStream),
	read_rules(InputStream,OutputStream,0,1,0).

% Rule for loading part of a specific chunk
/*
load_chunk_partial(InputStream, NoRulesRead, FirstIncluded, LastIncluded) :-
	chunk_size(ChunkSize),
	NoRulesRead < ChunkSize,
    read(InputStream, T),
    ((T == end_of_file) -> 
		close(InputStream)
    ;
		NextNoRulesRead is NoRulesRead + 1,
		((NoRulesRead >= FirstIncluded, NoRulesRead <= LastIncluded) ->
			assert(T) ;	true
			),
        	load_chunk_partial(InputStream, NextNoRulesRead, FirstIncluded, LastIncluded)
    	).

load_chunk(Chunk,HistorySize,FutureSize, LastChunk) :-
	PreviousChunk is Chunk - 1,
	NextChunk is Chunk + 1,
	chunk_size(ChunkSize),
	% Load history from previous chunk
	((PreviousChunk > 0) ->
		chunk_file_name(PreviousChunk, PreviousChunkFile),
		open(PreviousChunkFile,PreviousChunkStream),
		PreviousFirstInclude is ChunkSize - HistorySize,
		load_chunk_partial(PreviousChunkStream, PreviousFirstInclude, ChunkSize)
	),
	% Load current chunk
	chunk_file_name(Chunk,ChunkFile),
	open(ChunkFile,ChunkStream),
	load_chunk_partial(ChunkStream,1,ChunkSize),
	% Load future from next chunk
	chunk_file_name(NextChunk,NextChunkFile),
	open(NextChunkFile,NextChunkStream),
	load_chunk_partial(NextChunkStream,1,FutureSize).
*/

% Read a CHR store from a stream
load_store(InputStream) :-
    read(InputStream, T),
    ((T == end_of_file) -> 
		close(InputStream)
    ;
		%write('inserting constraint: '),
		write_canonical(T),nl,
		T,
		load_store(InputStream)
    ).

	
