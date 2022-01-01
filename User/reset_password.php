<?php
// Initialize the session
session_start();
 
// Check if the user is logged in, if not then redirect to login page
if(!isset($_SESSION["loggedin"]) || $_SESSION["loggedin"] !== true){
    header("location: login.php");
    exit;
}

// Include connectMysql file
require_once "connectMysql.php";
 
// Define variables and initialize with empty values
$new_password = $confirm_password = "";
$new_password_err = $confirm_password_err = "";
 
// Processing form data when form is submitted
if($_SERVER["REQUEST_METHOD"] == "POST"){
 
    // Validate new password
    if(empty(trim($_POST["new_password"]))){
        $new_password_err = "Please enter the new password.";     
    } elseif(strlen(trim($_POST["new_password"])) < 6){
        $new_password_err = "Password must have atleast 6 characters.";
    } else{
        $new_password = trim($_POST["new_password"]);
    }
    
    // Validate confirm password
    if(empty(trim($_POST["confirm_password"]))){
        $confirm_password_err = "Please confirm the password.";
    } else{
        $confirm_password = trim($_POST["confirm_password"]);
        if(empty($new_password_err) && ($new_password != $confirm_password)){
            $confirm_password_err = "Password did not match.";
        }
    }
        
    // Check input errors before updating the database
    if(empty($new_password_err) && empty($confirm_password_err)){
        // Prepare an update statement
        $sql = "UPDATE users SET password = ? WHERE id = ?";
        
        if($stmt = mysqli_prepare($db_link, $sql)){
            // Bind variables to the prepared statement as parameters
            mysqli_stmt_bind_param($stmt, "si", $param_password, $param_id);
            
            // Set parameters
            $param_password = password_hash($new_password, PASSWORD_DEFAULT);
            $param_id = $_SESSION["id"];
            
            // Attempt to execute the prepared statement
            if(mysqli_stmt_execute($stmt)){
                // Password updated successfully. Destroy the session, and redirect to login page
                session_destroy();
                header("location: login.php");
                exit();
            } else{
                echo "Oops! Something went wrong. Please try again later.";
            }
        }
        
        // Close statement
        mysqli_stmt_close($stmt);
    }
    
    // Close connection
    mysqli_close($db_link);
}
?>
 
<style type="text/css">
    /* visible and invisible image */
    img{
        width: 40px;
        height: 25px;
        position: relative;
        top: -30px;
        right: -200px;
    }
</style>

<!DOCTYPE html>
<html lang="zh-Hant-TW">
<head>
    <meta charset="UTF-8">
    <title>Reset Password</title>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.css">
    <style type="text/css">
        body{ font: 18px sans-serif; }
        .wrapper{ width: 350px; padding: 20px; }
    </style>
</head>
<body>
    <CENTER>
        <div class="wrapper">
            <h1>Reset Password</h1>
            <br> </br>
            <br> </br>
            <br> </br>

            <form action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>" method="post"> 
                <div class="form-group <?php echo (!empty($new_password_err)) ? 'has-error' : ''; ?>">
                    <label>New Password</label>
                    <input type="password" name="new_password" class="form-control" placeholder = "Password" id="demo_input" value="<?php echo $new_password; ?>">
                    <img id="demo_img" onclick="hideShowPsw()" src="visible.png">
                    <span class="help-block"><?php echo $new_password_err; ?></span>
                </div>
                <div class="form-group <?php echo (!empty($confirm_password_err)) ? 'has-error' : ''; ?>">
                    <label>Confirm Password</label>
                    <input type="password" name="confirm_password" class="form-control" placeholder = "Confirm Password" id="demo_input_2">
                    <img id="demo_img_2" onclick="hideShowPsw_2()" src="visible.png">
                    <span class="help-block"><?php echo $confirm_password_err; ?></span>
                </div>
                <br> </br>
                <div class="form-group">
                    <input type="submit" class="btn btn-primary" value="Submit">
                    <a class="btn btn-link" href="welcome.php">Cancel</a>
                </div>
            </form>
        </div>    
    </CENTER>

<script type="text/javascript">
	// javascript 
	var demoImg = document.getElementById("demo_img");
	var demoInput = document.getElementById("demo_input");

    //hidden text block, show password block
	function hideShowPsw(){
		if (demoInput.type == "password") {
			demoInput.type = "text";
			demo_img.src = "invisible.png";
		}else {
			demoInput.type = "password";
			demo_img.src = "visible.png";
		}
	}


    var demoImg_2 = document.getElementById("demo_img_2");
	var demoInput_2 = document.getElementById("demo_input_2");

    //hidden text block, show password block
    function hideShowPsw_2(){
        if (demoInput_2.type == "password") {
			demoInput_2.type = "text";
			demo_img.src = "invisible.png";
		}else {
			demoInput_2.type = "password";
			demo_img_2.src = "visible.png";
		}
    }
    
</script>

</body>
</html>
<?php


