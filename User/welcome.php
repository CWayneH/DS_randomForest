<?php
// Initialize the session
session_start();
 
// Check if the user is logged in, if not then redirect him to login page
if(!isset($_SESSION["loggedin"]) || $_SESSION["loggedin"] !== true){
    header("location: login.php");
    exit;
}
if($_SESSION["username"] !== "kebwlmbhee"){
    header("location: welcome2.php");
    exit;
}
?>
 
<!DOCTYPE html>
<!-- lang 代表網頁主要語言 -->
<html lang="zh-Hant-TW">
<head>
    <meta charset="UTF-8">
    <title>Welcome</title>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.css">
    <style type="text/css">
        body{ font: 14px sans-serif; text-align: center; }
    </style>
</head>
<body>
    
        <h1>Hi, <b><?php echo htmlspecialchars($_SESSION["username"]); ?></b>. Welcome !!</h1>
    

</html>


<?php
// 載入 connectMysql.php 連接資料庫
require_once 'connectMysql.php';
?>

<head>
    <meta charset="UTF-8" />
    <title> DB_Project_自動氣象站 </title>
<head>

<body>
    <CENTER>
        <H1>
            <!-- title -->
            <font size="8"><b>自動氣象站</b></font><br><br>

            <a href="read.php">      READ </a>
            <br> </br>
            <a href="create.php">   INSERT </a>
            <br> </br>
            <a href="update.php">   UPDATE </a>
            <br> </br>
            <a href="delete.php">   DELETE </a>
            <br> </br>
            <a href="query.php">Query</a>
        </H1>
    </CENTER>
</body>

<body>
<p>
    <br></br>
        <a href="reset_password.php" class="btn btn-warning">Reset Your Password</a>
        <a href="logout.php" class="btn btn-danger">Sign Out of Your Account</a>
    </p>
</body>