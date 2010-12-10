-- Main package for KOW_View
--
-- author: Eduardo Calderini Jr <ecalderini@ydeasolutions.com.br>


pragma License (Modified_GPL);

-------------------
-- KOW Framework --
-------------------
with KOW_Sec.Accounting;

package KOW_View is
	Accountant : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "kow_sec" );
end KOW_View;
