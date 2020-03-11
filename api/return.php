<?php

declare(strict_types=1);

include_once 'functions.php';

$tokens = token_get_all('<?php add(multiply(2, 3), 5);');
var_dump(
    array_map(
        fn ($t) => is_array($t) ? sprintf('%s : %s', token_name($t[0]), $t[1]) : $t,
        $tokens
    )
);