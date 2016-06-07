TRIGGER PROCEDURE FOR DELETE OF PartVendorLink.

{trgValidateSession.i}

{trgCreateAudit.i "PartVendorLink" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Part 
              WHERE Part.PartID = PartVendorLink.PartID) 
AND CAN-FIND(FIRST Vendor 
              WHERE Vendor.VendorID = PartVendorLink.VendorID) 
THEN
DO:
   RETURN ERROR "Part and Vendor exist for this PartVendorLink. Cannot Delete.".
END.
