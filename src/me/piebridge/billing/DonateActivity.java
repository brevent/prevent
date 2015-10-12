package me.piebridge.billing;

import android.app.Activity;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender;
import android.os.Bundle;
import android.os.RemoteException;
import android.widget.Toast;

import com.android.vending.billing.IInAppBillingService;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;

import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/11.
 */
public abstract class DonateActivity extends Activity implements DonateListener {

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == DonateUtils.REQUEST_CODE && resultCode == RESULT_OK) {
            UILog.d(data.getStringExtra("INAPP_PURCHASE_DATA"));
            if (DonateUtils.isSignature(data.getStringExtra("INAPP_DATA_SIGNATURE"))) {
                onDonated(null);
            }
        }
    }

    private boolean checkDonate(IInAppBillingService service) {
        try {
            Bundle sku = new Bundle();
            ArrayList<String> skuList = new ArrayList<String>();
            skuList.add(DonateUtils.ITEM_ID);
            sku.putStringArrayList("DETAILS_LIST", skuList);
            Bundle skuDetails = service.getSkuDetails(DonateUtils.API_VERSION, getPackageName(), DonateUtils.ITEM_TYPE, sku);
            if (skuDetails.getInt("RESPONSE_CODE") != 0) {
                Toast.makeText(this, "response code: " + skuDetails.getInt("RESPONSE_CODE"), Toast.LENGTH_LONG).show();
                return false;
            }
            ArrayList<String> detailsList = skuDetails.getStringArrayList("DETAILS_LIST");
            if (detailsList == null || detailsList.isEmpty()) {
                Toast.makeText(this, "cannot find sku details", Toast.LENGTH_LONG).show();
                return false;
            }
            ((android.text.ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE)).setText(detailsList.toString());
            for (String details : detailsList) {
                JSONObject object = new JSONObject(details);
                if (DonateUtils.ITEM_ID.equals(object.get("productId"))) {
                    return true;
                }
            }
        } catch (RemoteException e) {
            UILog.d("cannot get sku details", e);
        } catch (JSONException e) {
            UILog.d("cannot get json", e);
        }
        return false;
    }

    private void donate(IInAppBillingService service) {
        try {
            Bundle bundle = service.getBuyIntent(DonateUtils.API_VERSION, getPackageName(), DonateUtils.ITEM_ID,
                    DonateUtils.ITEM_TYPE, DonateUtils.ITEM_ID);
            PendingIntent intent = bundle.getParcelable("BUY_INTENT");
            if (intent != null) {
                startIntentSenderForResult(intent.getIntentSender(), DonateUtils.REQUEST_CODE, new Intent(), 0, 0, 0);
            } else {
                UILog.d("cannot get buy intent");
            }
        } catch (RemoteException e) {
            UILog.d("cannot get buy intent", e);
        } catch (IntentSender.SendIntentException e) {
            UILog.d("cannot start buy intent", e);
        }
    }

    public boolean donateViaPlay() {
        final Intent serviceIntent = new Intent("com.android.vending.billing.InAppBillingService.BIND");
        serviceIntent.setPackage("com.android.vending");
        bindService(serviceIntent, new DonateService(this) {
            @Override
            protected boolean isDonated() {
                return false;
            }

            @Override
            protected void onAvailable(IInAppBillingService service) {
                if (checkDonate(service)) {
                    donate(service);
                }
            }
        }, Context.BIND_AUTO_CREATE);
        return false;
    }

    public void checkDonate() {
        Intent serviceIntent = new Intent("com.android.vending.billing.InAppBillingService.BIND");
        serviceIntent.setPackage("com.android.vending");
        bindService(serviceIntent, new DonateService(this), Context.BIND_AUTO_CREATE);
    }

    @Override
    public void onAvailable(IInAppBillingService service) {

    }

    @Override
    public void onDonated(IInAppBillingService service) {

    }

    @Override
    public void onUnavailable(IInAppBillingService service) {

    }

}
