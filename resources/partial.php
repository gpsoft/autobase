{{=(@ @)=}}
<?php
/**
 * (@ent-name@)管理共通
 * Copyright: (@this-year@)
 *
 */

define('PROJ_NAME', '(@proj-name@)');
define('TABLE_NAME', '(@table-name@)');

// 画面ID's
(@#disp-ids@)
//   (@{disp-id}@)
(@/disp-ids@)
(@#detail?@)
// Notice we need D page.
(@/detail?@)


$aryMap = Array(
(@#properties@)
	(@{prop-name-to-col-name}@),
(@/properties@)
);

$aryMap2 = Array(
(@#properties@)
	(@{col-name-to-var-name}@),
(@/properties@)
);

$aryMap3 = Array(
(@#properties@)
	(@{hid-name-to-upsert}@),
(@/properties@)
);

$aryMap4 = Array(
(@#properties@)
	(@{col-name-to-param-name}@),
(@/properties@)
);

// for U page.
(@#properties@)
(@#edit-section@)
(@.@)
(@/edit-section@)
(@/properties@)

// for S page.
(@#properties@)
(@#search-section@)
(@.@)
(@/search-section@)
(@/properties@)
