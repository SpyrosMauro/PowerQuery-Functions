# powerquery
A collection of functions written in Power Query (M)
*Or "I paste stuff here sometimes"*


## What you'll find:

## Table Manipulation
| Function | Description |
| :--- | ----------- |
| [Table.CombineMultipleColumns](Tables/Table.CombineMultipleColumns.pq) | For double headers. Combine columns by any <code>setSize</code> in one step. |
| [Table.GenerateColumns](Tables/Table.GenerateColumns.pq) | To add multiple columns based on similar functions.<p>*Have you thought about just unpivoting and adding one column afterwards?* |
| [Table.InferColumnTypes](Tables/Table.InferColumnTypes.pq) | Infer column types.<p>**Only primitive types supported.** |
| [Table.ReplaceValuesFromTable](Tables/Table.ReplaceValuesFromTable.pq) | Optimised mass replace values in columns. |
| [Table.ToTableView](Tables/Table.ToTableView.pq) | Automatically apply a <code>Table.View</code> function based on <code>Table.Schema</code>.<p>**Only primitive types supported.** |

## Time Intelligence
| Function | Description |
| :--- | ----------- |
| [DateTime.Diff](Time%20Intelligence/DateTime.Diff.pq) | DateTime Diff using <code>Excluded Dates</code> list and <code>Start Time</code> / <code>End Time</code>. |
| [Date.Diff](Time%20Intelligence/Date.Diff.pq) | Date Diff using <code>Excluded Dates</code> list. |
| [Duration.To](Time%20Intelligence/Duration.To.pq) | All Duration.Total*xxxx* functions in one. |

## Model
| Function | Description |
| :--- | ----------- |
| [PowerBI.Current](Model/PowerBI.Current.pq) | Connects to current PBI model. Requires <code>userName</code>.<p> Needs <b>exactly one</b> Power BI Desktop instance running.<p><i>Does not work in Power BI Service.</i> |


## Misc
| Function | Description |
| :--- | ----------- |
| [Get.TableView](Other/Get.TableView.pq) | Get formatted <code>Table.View</code> function based on <code>Table.Schema</code> or by infering types.<p>**Only primitive types supported.** |
| [Types.TableType](Types/Type.TableType.pq) | Get text of non abstract table type. |