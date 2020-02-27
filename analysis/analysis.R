analysis.ilp = function (sample, csvfile, outputfile)
{
	data = read.csv(csvfile);
	n = length(data[,1]);
	m = 0;
	pcorrect = 0;
	TP = 0;
	TN = 0;
	FP = 0;
	FN = 0;
	scallop1 = 0;
	scallop0 = 0;
	ILP1 = 0;
	ILP0 = 0;
	predict1 = 0;
	predict0 = 0;
	best1 = 0;
	best0 = 0; 
	for (k in seq(1, n))
	{
		if(data[k, "True.scallop"] == data[k, "True.ILP"] && data[k, "False.scallop"] == data[k, "False.ILP"])
		{
			next;
		}
		m = m + 1;
		if(data[k, "prediction"] == data[k, "Count.Label"] && data[k, "prediction"] == 1)
		{
			TP = TP + 1;
		}
		if(data[k, "prediction"] == data[k, "Count.Label"] && data[k, "prediction"] == 0)
		{
			TN = TN + 1;
		}
		if(data[k, "prediction"] != data[k, "Count.Label"] && data[k, "prediction"] == 1)
		{
			FP = FP + 1;
		}
		if(data[k, "prediction"] != data[k, "Count.Label"] && data[k, "prediction"] == 0)
		{
			FN = FN + 1;
		}
		scallop1 = scallop1 + data[k, "True.scallop"];
		scallop0 = scallop0 + data[k, "False.scallop"];
		ILP1 = ILP1 + data[k, "True.ILP"];
		ILP0 = ILP0 + data[k, "False.ILP"];
		predict1 = predict1 + data[k, "True.predict"];
		predict0 = predict0 + data[k, "False.predict"];
		best1 = best1 + data[k, "True.best"];
		best0 = best0 + data[k, "False.best"];
	}

	ref = 1996.69;
	x = sprintf("%s, n = %d, m = %d, TP = %d, TN = %d, FP = %d, FN = %d, Scallop = (%.2f, %.2f), ILP = (%.2f, %.2f), best = (%.2f, %.2f), predict = (%.2f, %.2f)", 
			sample, n, m, TP, TN, FP, FN, 
			scallop1 / ref, scallop1 * 100 / (scallop0 + scallop1), 
			ILP1 / ref, ILP1 * 100 / (ILP0 + ILP1), 
			best1 / ref, best1 * 100 / (best0 + best1), 
			predict1 / ref, predict1 * 100 / (predict0 + predict1));
	write(x, outputfile, append = TRUE);
}

analysis.ilp("SRR307903", "../v2/SRR307903/prediction.csv", "results")
analysis.ilp("SRR307911", "../v2/SRR307911/prediction.csv", "results")
analysis.ilp("SRR315323", "../v2/SRR315323/prediction.csv", "results")
analysis.ilp("SRR315334", "../v2/SRR315334/prediction.csv", "results")
analysis.ilp("SRR387661", "../v2/SRR387661/prediction.csv", "results")
analysis.ilp("SRR534291", "../v2/SRR534291/prediction.csv", "results")
analysis.ilp("SRR534307", "../v2/SRR534307/prediction.csv", "results")
analysis.ilp("SRR534319", "../v2/SRR534319/prediction.csv", "results")
analysis.ilp("SRR545695", "../v2/SRR545695/prediction.csv", "results")
analysis.ilp("SRR545723", "../v2/SRR545723/prediction.csv", "results")
