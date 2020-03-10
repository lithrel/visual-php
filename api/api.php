<?php

declare(strict_types=1);

namespace VisualPHP;

include 'functions.php';
$whiteList = [
    'add',
    'multiply',
];


$declared = get_defined_functions(true);
$functions = [];
foreach ($declared['user'] as $fName) {
    if (!in_array($fName, $whiteList)) {
        continue;
    }

    $ref = new \ReflectionFunction($fName);

    $functions[] = [
        'name' => $fName,
        'type' => (string) $ref->getReturnType(),
        'params' => array_map(
            fn($p) => getParamDefinition($p), 
            $ref->getParameters()
        ),
        'source' => getSource
            ($ref->getFileName(),
            $ref->getStartLine(),
            $ref->getEndLine()
        ),
    ];
}

$ret = json_encode($functions, \JSON_PRETTY_PRINT);

print_r($ret);

function getParamDefinition(\ReflectionParameter $p): array
{
    return [
        'name' => '$' . $p->getName(),
        'type' => (string) $p->getType(),
        'default' => $p->isDefaultValueAvailable() ? $p->getDefaultValue() : null,
    ];
}

function getSource(string $file, int $lineStart, int $lineEnd): string
{
    --$lineStart; // to get func declaration
    $length = $lineEnd - $lineStart;
    $source = file($file, \FILE_IGNORE_NEW_LINES);
    $lines = array_slice($source, $lineStart, $length, true);
    return implode ("\n", $lines);
}