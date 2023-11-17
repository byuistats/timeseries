<?php
if ($_SERVER['REQUEST_METHOD'] === 'GET') {
    if (!is_null($_GET['link']) && !empty($_GET['link'])) {
        $file = $_GET['link'];
        try {
            header('Content-type: image/*');
            header("Content-Disposition: attachment; filename=\"$file\"");
            readfile($file);
        }
        catch (Exception $ex){
            echo $ex->getMessage();
        }
    }
}