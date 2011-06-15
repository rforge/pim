PIM.model.matrix <-
function(form, data)
{
	data.tmp <- PIM.Data(data)
	form.tmp <- update(form, PO ~.)
	Z <- model.matrix(form.tmp, data = data.tmp)
	return(list(data.tmp = data.tmp, Z = Z))
}

