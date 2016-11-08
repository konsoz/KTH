function validateName() {
        var nameRegex = /^.*\d+.*$/g;
        var name = $(".nameText").val();
        var invalidMessage = $(".invalidNameMessage").hide();
        var emptyMessage = $(".emptyNameMessage").hide();
        var validation = nameRegex.test(name);
        if(!name) {
            emptyMessage.show();
        } else if(validation || name.length <= 1 || name.length > 30) {
            invalidMessage.show();
	} 
} 

function validateSurname() {
        var nameRegex = /^.*\d+.*$/g;
        var name = $(".surnameText").val();
        var invalidMessage = $(".invalidSurnameMessage").hide();
        var emptyMessage = $(".emptySurnameMessage").hide();
        var validation = nameRegex.test(name);
        if(!name) {
            emptyMessage.show();
        } else if(validation || name.length <= 1 || name.length > 30) {
            invalidMessage.show();
	}
} 

function validateSSN() {
	var ssnReg = /^[12]{1}[90]{1}[0-9]{6}-[0-9]{4}$/g;
        var ssn = $(".ssnText").val();
        var validation = ssnReg.test(ssn);
        var invalidMessage = $(".invalidSSNMessage").hide();
        var emptyMessage = $(".emptySSNMessage").hide();
        if(!ssn) {
            emptyMessage.show();
        } else if(!validation) {
            invalidMessage.show();
	} 
} 

function validateEmail() {
	var emailReg = /^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$/g;
        var email = $(".mailText").val();
        var validation = emailReg.test(email);
        var invalidMessage = $(".invaldEmailMessage").hide();
        var emptyMessage = $(".emptyEmailMessage").hide();
        if(!email) {
            emptyMessage.show();
        } else if(!validation) {
            invalidMessage.show();
	} 
} 

function validateUsername() {
        var usernameRegex = /^[a-zA-Z0-9]{6,}$/g;
        var name = $(".usernameText").val();
        var invalidMessage = $(".invalidUsernameMessage").hide();
        var emptyMessage = $(".emptyUsernameMessage").hide();
        var validation = usernameRegex.test(name);
        if(!name) {
            emptyMessage.show();
        } else if(!validation) {
            invalidMessage.show();
	} 
}

function validatePassword() {
        var usernameRegex = /^[a-zA-Z0-9]{6,}$/g;
        var name = $(".passwordText").val();
        var invalidMessage = $(".invalidPasswordMessage").hide();
        var emptyMessage = $(".emptyPasswordMessage").hide();
        var validation = usernameRegex.test(name);
        if(!name) {
            emptyMessage.show();
        } else if(!validation) {
            invalidMessage.show();
	} 
} 

function validateFromDate() {
    var dateRegex = /^(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[012])-(19|20)\d\d$/g;
    var name = $(".fromDateText").val();
    var invalidMessage = $(".invalidFromDateMessage").hide();
    var emptyMessage = $(".emptyFromDateMessage").hide();
    var validation = dateRegex.test(name);
    if(!name) {
        emptyMessage.show();
    } else if(!validation) {
        invalidMessage.show();
    } 
}

function validateToDate() {
    var dateRegex = /^(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[012])-(19|20)\d\d$/g;
    var name = $(".toDateText").val();
    var invalidMessage = $(".invalidToDateMessage").hide();
    var emptyMessage = $(".emptyToDateMessage").hide();
    var validation = dateRegex.test(name);
    if(!name) {
        emptyMessage.show();
    } else if(!validation) {
        invalidMessage.show();
    } 
}