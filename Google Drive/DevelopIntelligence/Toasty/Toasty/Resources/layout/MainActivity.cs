using Android.App;
using Android.Widget;
using Android.OS;

namespace Toasty
{
	[Activity(Label = "Toasty", MainLauncher = true)]
	public class MainActivity : Activity
	{
		protected override void OnCreate(Bundle savedInstanceState)
		{
			base.OnCreate(savedInstanceState);

			// Set our view from the "main" layout resource
			SetContentView(Resource.Layout.Main);

			// Get our button from the layout resource and attach an event to it
			Button button = FindViewById<Button>(Resource.Id.submit_name);

			EditText editText = FindViewById<EditText>(Resource.Id.first_name);

			button.Click += delegate { Toast.MakeText(this, editText.Text, ToastLength.Long).Show(); };

		}
	}
}

